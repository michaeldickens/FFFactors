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
import qualified Data.Set as Set
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
  bm <- retsFromFile "French/Portfolios_B-M_Value_Wt.csv" [("Hi 30", 1), ("Lo 30", -1)]
  ep <- retsFromFile "French/Portfolios_E-P_Value_Wt.csv" [("Hi 30", 1), ("Lo 30", -1)]
  cfp <- retsFromFile "French/Portfolios_CF-P_Value_Wt.csv" [("Hi 30", 1), ("Lo 30", -1)]
  op <- retsFromFile "French/Portfolios_Profitability_Value_Wt.csv" [("Hi 30", 1), ("Lo 30", -1)]

  putStrLn "Post-2007 returns:"
  for_ [(bm, "B/M "), (ep, "E/P "), (cfp, "CF/P")] $ \(factor, name) -> do
    printf "%s: %.3f and %.3f\n" name
      (annualizedReturn $ startingPeriod 2007 1 factor)
      (annualizedReturn $ startingPeriod 2007 1 $ beforePeriod 2014 1 factor)
