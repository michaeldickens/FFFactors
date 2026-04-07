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


main :: IO ()
main = do
  rf <- loadRF
  ff3 <- loadDB "French/3_Factors.csv"
  aavm <- retsFromFile "AA_Sim.csv" [("AAVM", 1)]
  trend <- retsFromFile1 "Trend_Index.csv" "Trend Index"
  let equities = getRets [("Mkt-RF", 1), ("RF", 1)] ff3

  let assets = [(equities, 1), (trend, 1)]

  let res0 = leverageToleranceRebalancing 0 assets rf
  let res1 = leverageToleranceRebalancing 0.1 assets rf
  let res2 = leverageToleranceRebalancing 0.2 assets rf
  printStatsOrg " 0%" res0
  printStatsOrg "10%" res1
  printStatsOrg "20%" res2

  plotLineGraphLog "images/tolerance bands.png" "Tolerance Rules – Returns" "CAGR"
    [ ("monthly", returnsToPrices res0)
    , ("10%", returnsToPrices res1)
    , ("20%", returnsToPrices res2)
    ]

  plotLineGraph "images/tolerance bands drawdowns.png" "Tolerance Rules – Drawdowns" "Drawdown"
    [ ("monthly", apply drawdowns res0)
    , ("10%", apply drawdowns res1)
    , ("20%", apply drawdowns res2)
    ]
