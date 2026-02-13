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


main = do
  allFutures <- loadDB "HLWZ/Asset_Returns.csv"
  vixQ <- loadDailyPriceDB "VIX.csv"
  rf <- loadRF
  let vix = (* 17.24) $ returnsToPrices $ getRets1 "close" vixQ

  let prefixes = ["Equity", "Bond", "Commodity", "Currency"]
  let futuresByClass = for prefixes $ \prefix ->
        flip Map.map allFutures (
        Map.filterWithKey (
            \k _ -> Text.pack prefix `Text.isPrefixOf` k
            )
        )

  let mfByClass = for futuresByClass $ managedFutures' 20 TMOM 12 rf
  mf <- managedFutures TMOM 12
