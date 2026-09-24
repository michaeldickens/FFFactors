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
import Statistics.Regression
import System.IO
import Text.Printf


shittyManFut :: IO ()
shittyManFut = do
  equityQuotes <- loadDB "French/3_Factors.csv"
  bondQuotes <- loadDB "10Y_Treasury_Returns.csv"
  commodityQuotes <- loadDB "AQR/Commodities.csv"
  rf <- loadRF

  let equityRets = getRets [("Mkt-RF", 1), ("RF", 1)] equityQuotes
  let bondRets = getRets1 "Return M" bondQuotes
  let commodityRets = getRets [("Excess return of equal-weight commodities portfolio", 1), ("RF", 1)] $ mergeQuoteMaps commodityQuotes equityQuotes

  let quoteMap :: QuoteMap
      quoteMap =
        Map.fromList $ map (\k -> (k, Map.fromList
              [ (Text.pack "EQ", Just $ equityRets!k)
              , (Text.pack "FI", Just $ bondRets!k)
              , (Text.pack "CM", Just $ commodityRets!k)
              ]
            )) $ jointDateRange [equityRets, bondRets, commodityRets]


  let rets = managedFutures' 20 TMOM 12 rf quoteMap
  -- putStrLn $ intercalate "\n" $ map (\(k, v) -> printf "%s\t%.2f" (show k) (100 * v)) $ sort $ Map.toList rets
  printStatsOrg "MF" rets

  sg' <- retsFromFile1 "Trend_Index.csv" "Trend Index"
  let [sg, overlap] = fixDates [sg', rets]

  putStrLn ""
  printStatsOrg "Trend Index" sg
  printStatsOrg "Coarse MF" overlap
  print $ correlation sg overlap
  print $ minMaxDates overlap
  printFactorRegression overlap rf [sg] ["Trend Index"]
  printFactorRegression sg rf [overlap] ["Coarse MF"]


main = do
  usQ <- loadDB "French/3_Factors.csv"
  tsmomQ <- loadDB "AQR/TSMOM.csv"

  let tsmomNames = ["TSMOM^EQ", "TSMOM^CM", "TSMOM^FI", "TSMOM^FX"]
  let usRets = getRets1 "Mkt-RF" usQ
  let tsmomRetses = map (\k -> imposeCost 0.05 $ getRets1 k tsmomQ) tsmomNames

  let cfg = mvoFactorConfig { riskMetric = stdev }
  -- let cfg = mvoFactorConfig { riskMetric = ulcerIndex }
  printMVO cfg (fixDates $ usRets:tsmomRetses) ("EQ":tsmomNames)
