{-# OPTIONS_GHC
    -fno-warn-unused-binds
    -fno-warn-unused-imports
    -fno-warn-missing-signatures
    -fno-warn-unused-matches
    -fno-warn-x-partial
    -fno-warn-compat-unqualified-imports
#-}
{- |
Module      : Main
Description :

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2018-10-12

-}

module Main (main) where

import ChartPlot
import FFFactors
import MVO

import MaybeArithmetic
import Returns
import Period

import Control.Monad
import Data.Foldable (for_)
import Data.Function (on)
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LA
import qualified Statistics.Distribution as Dist
import qualified Statistics.Distribution.StudentT as StudentT
import Statistics.Regression
import System.IO
import Text.Printf


-- | Equal-weight all segments in a given list such that the weights sum to 1.
equally :: [a] -> [(a, Double)]
equally xs = zip xs $ repeat (1 / fromIntegral (length xs))


globalMom :: IO ()
globalMom = do
  -- this function is old, there are simpler ways to do some of this stuff now.
  -- I should rewrite it
  usQuotes <- loadDB "French/3_Factors.csv"
  exQuotes <- loadDB "French/3_Factors_Developed_ex_US.csv"
  euQuotes <- loadDB "French/3_Factors_Europe.csv"
  jpQuotes <- loadDB "French/3_Factors_Japan.csv"
  asQuotes <- loadDB "French/3_Factors_Asia_Pacific_ex_Japan.csv"
  emQuotes <- loadDB "French/5_Factors_Emerging.csv"
  coQuotes <- loadDB "AQR/Commodities.csv"
  bdQuotes <- loadDB "10Y_Treasury_Returns.csv"

  let allRets = Map.fromList [
        ("us", getRets [("Mkt-RF", 1), ("RF", 1)] usQuotes),
        ("ex", getRets [("Mkt-RF", 1), ("RF", 1)] exQuotes),
        ("eu", getRets [("Mkt-RF", 1), ("RF", 1)] euQuotes),
        ("jp", getRets [("Mkt-RF", 1), ("RF", 1)] jpQuotes),
        ("as", getRets [("Mkt-RF", 1), ("RF", 1)] asQuotes),
        ("em", getRets [("Mkt-RF", 1), ("RF", 1)] emQuotes),
        ("co", getRets [("Excess return of equal-weight commodities portfolio", 1)
                       , ("Excess return of long/short commodities portfolio", 1/3)
                       , ("RF", 1)
                       ] coQuotes),
        ("bd", getRets [("Return M", 1)] bdQuotes)
        ]

  let gemReturns :: Int -> IO (RetSeries)
      gemReturns lookbackMonths = do
        let dateRange :: [Period]
            dateRange = jointDateRange $ Map.elems allRets
        let dateRangeAfterOneYear = drop (max 12 lookbackMonths) dateRange
        let rf = getRets1 "RF" usQuotes

        let trendRets = Map.map (\m -> trendOverlay TMOM lookbackMonths rf m m) allRets

        let gem :: Map.HashMap String (RetSeries) -> Int ->
                   RetSeries
            gem retsBase numSegmentsToHold = Map.fromList $ zip dateRange $ map
              (\period -> let topSegments = take numSegmentsToHold $ reverse
                                $ sortBy
                                (compare `on`
                                  (\segment -> fromJust $ tsMomentum lookbackMonths period (allRets Map.! segment)
                                  )) $ Map.keys allRets
                          in average $ map (\segment -> retsBase Map.! segment Map.! period) topSegments
              ) dateRangeAfterOneYear

        let gemNoTrend = gem allRets
        let gemTrend = gem trendRets

        return $ gemTrend 1

  gemReturns 12 >>= printStatsOrg "12-month" . startingPeriod 2015 1


main = do
  rf <- loadRF
  beta <- retsFromFile1 "French/3_Factors.csv" "Mkt-RF"
  aaQ <- loadDB "AA_Sim.csv"
  allFutures <- loadDB "HLWZ/Asset_Returns.csv"
  gfp <- loadDB "Global_Factor_Premiums.csv"
  let mkt' = rf + beta
  let gfpNames = ["Trend^EQ", "Trend^FI", "Trend^CM", "Trend^FX", "Trend^MA", "Mom^EQ", "Val^EQ"]
  let gfpFactors' = map (flip getRets1 gfp) gfpNames

  let futures = flip Map.map allFutures (
        Map.filterWithKey (
            \k _ -> Text.pack "Bond_" `Text.isPrefixOf` k ||
                    Text.pack "Commodity_" `Text.isPrefixOf` k
            )
        )

  let mf1 = managedFutures' 60 TMOM 5 rf futures
  let mf2 = managedFutures' 60 SMA 5 rf futures
  let [mf, aavm, aavmTrend] =
          fixDates [ imposeCost 0.06 $ 0.5 * (mf1 + mf2) - rf
                   , imposeCost 0.02 $ getRets1 "AAVM" aaQ
                   , imposeCost 0.02 $ getRets1 "AAVM+Trend" aaQ
                   ]

  print $ minMaxDates mf
  printStatsOrg "AAVM only" aavm
  printStatsOrg "AAVM^T" aavmTrend
  printStatsOrg "MF only" $ rf + mf
  printStatsOrg "AAVM + MF" $ aavm + mf
  printStatsOrg "AAVM^T/2 + MF" $ 0.5 * (aavm + aavmTrend) + mf
  printStatsOrg "AAVM^T + MF" $ aavmTrend + mf
  printStatsOrg "AAVM^T + MF + 25% Mkt" $ aavmTrend + mf + 0.25 * beta

  let [mkt, trendEQ, trendFI, trendCM, trendFX, trendMA, momEQ, valEQ] = fixDates (mkt':gfpFactors')

  putStrLn ""
  print $ minMaxDates mkt
  let gmf = imposeCost 0.10 $ trendFI + trendCM
  let valmom = imposeCost 0.03 $ 0.4 * valEQ + 0.4 * momEQ
  let overlay = imposeCost 0.01 $ 0.5 * trendEQ
  printStatsOrg "~AAVM only" $ mkt + valmom
  printStatsOrg "~AAVM^T" $ 0.5 * mkt + overlay + valmom
  printStatsOrg "~AAVM + MF" $ mkt + valmom + gmf
  printStatsOrg "~AAVM^T + MF" $ 0.5 * mkt + overlay + valmom + gmf

  plotLineGraphLog "images/AAVM with MF.png" "AAVM Configurations" "Price"
    [ ("AAVM + MF", returnsToPrices $ aavm + mf)
    , ("AAVM^T + MF", returnsToPrices $ aavmTrend + mf)
    ]

  plotLineGraph "images/AAVM with MF drawdowns.png" "Drawdowns for AAVM Configurations" "Drawdown"
    [ ("AAVM + MF", apply drawdowns $ aavm + mf)
    , ("AAVM^T + MF", apply drawdowns $ aavmTrend + mf)
    ]

  plotLineGraphLog "images/AAVM-GFP with MF.png" "AAVM Configurations" "Price"
    [ ("~AAVM + MF", returnsToPrices $ mkt + valmom + gmf)
    , ("~AAVM^T + MF", returnsToPrices $ 0.5 * mkt + overlay + valmom + gmf)
    ]

  plotLineGraph "images/AAVM-GFP with MF drawdowns.png" "Drawdowns for AAVM Configurations" "Drawdown"
    [ ("~AAVM + MF", apply drawdowns $ mkt + valmom + gmf)
    , ("~AAVM^T + MF", apply drawdowns $ 0.5 * mkt + overlay + valmom + gmf)
    ]
