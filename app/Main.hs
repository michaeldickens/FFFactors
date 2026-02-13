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
  usBeta <- retsFromFile1 "French/3_Factors.csv" "Mkt-RF"
  devBeta <- retsFromFile1 "French/3_Factors_Developed_ex_US.csv" "Mkt-RF"
  gfp <- loadDB "Global_Factor_Premiums.csv"
  let trendEQ' = getRets1 "Trend^EQ" gfp


  let trendy numMonths =
        longShortTrend 7.5 LaggedTMOM numMonths 0 usBeta
        + longShortTrend 7.5 LaggedTMOM numMonths 0 devBeta

  let [myTrend6, myTrend, myTrend13, trendEQ] =
        fixDates [trendy 6, trendy 12, trendy 13, trendEQ']

  print $ minMaxDates myTrend
  printStatsOrg "Trend^EQ" $ trendEQ + rf
  printStatsOrg "My Trend (12)" $ myTrend + rf
  printStatsOrg "My Trend (13)" $ myTrend13 + rf
  printStatsOrg "My Trend  (6)" $ myTrend6 + rf
  printFactorRegression trendEQ 0 [myTrend, myTrend13] ["My12", "My13"]

  plotLineGraph "images/temp.png" "GFP Trend^EQ vs. Replication" "Price"
    [ ("GFP", returnsToPrices trendEQ)
    , ("Replication (12-mo)", returnsToPrices myTrend)
    , ("Replication (13-mo)", returnsToPrices myTrend13)
    , ("Replication (6-mo)", returnsToPrices myTrend6)
    ]

  let myTrend' = longShortTrend 10 LaggedTMOM 12 0 usBeta
  let [myTrend, trendEQ] = fixDates [myTrend', trendEQ']

  putStrLn ""
  print $ minMaxDates myTrend
  printStatsOrg "US Trend" $ myTrend + rf
  printStatsOrg "Trend^EQ" $ trendEQ + rf
