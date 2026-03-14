{-# OPTIONS_GHC
    -fno-warn-x-partial
#-}
{- |
Module      : Concentrated
Description : Replication code for "I was wrong, concentrated factor portfolios don't have alpha"

Maintainer  : Michael Dickens
Created     : 2026-02-10

-}

module Concentrated where

import Data.Foldable (for_)
import qualified Data.HashMap.Strict as Map
import Text.Printf

import FFFactors
import Regression
import Returns


myPrinter :: Bool -> QuoteMap -> String -> RetSeries -> RetSeries -> IO ()
myPrinter isLongShort ff3 metric ys xs = do
  let rf = getRets1 "RF" ff3
  let beta = getRets1 "Mkt-RF" ff3
  let mkt = beta + rf
  let smb = getRets1 "SMB" ff3
  let df = (Map.size $ head $ fixDates [beta, xs, ys]) - 4
  let (alpha:coefs, tstat:_, rsqr) =
        factorRegression ys (if isLongShort then 0 else rf)
        [beta, smb, if isLongShort then xs else xs - mkt]
  printf "| %s | %.2f | %.2f | %.2f | %.2f%%%s | %.2f |\n"
    metric
    (coefs!!0)
    (coefs!!1)
    (coefs!!2)
    (100 * ((1 + alpha)**12 - 1))
    (starsForPval $ pValue tstat df)
    tstat


main = do
  let includeShorts = False
  for_ [("", "US"), ("Developed_ex_US_", "Developed ex-US")] $ \(region, regionName) -> do
    ff3 <- loadDB (printf "French/%s3_Factors.csv" region)

    for_ [5, 4] $ \numSizes -> do
      -- as of 2025, $1B cutoff is top 8 deciles and $2B is top 7 deciles. see Ken
      -- French ME_Breakpoints (not downloaded)

      printf "\n**%s (%s)**\n\n"
        regionName
        (if numSizes == 4 then "realistic" else "all-cap")
      putStrLn "| | beta | SMB | factor | annual alpha | t-stat |"
      putStrLn "|-|"

      for_ [ ("B/M", "BM")
           , ("Momentum", "PRIOR")
           , ("Profitability", "OP")
           , ("Investment", "INV")
           ]
        $ \(metric, metric2) -> do
          let metric' = map (\c -> if c == '/' then '-' else c) metric

          -- Adjust for upward bias in equal-weighted returns. Bias depends on
          -- whether portfolio is rebalanced monthly or annually. See "Noisy
          -- Prices and Inference Regarding Returns"
          let biasAdjustment =
                if numSizes == 4
                then (if metric == "Momentum" then 0.23/100 else 0.23/12/100)
                else (if metric == "Momentum" then 0.4/100 else 0.4/12/100)

          let segments =
                map (\s -> printf s metric2)
                ["BIG Hi%s", "ME4 %s5", "ME3 %s5", "ME2 %s5", "SMALL Hi%s"]
          let segments40 =
                map (\s -> printf s metric2)
                ["ME5 %s4", "ME4 %s4", "ME3 %s4", "ME2 %s4", "ME1 %s4"]
          let segmentsShort =
                map (\s -> printf s metric2)
                ["BIG Lo%s", "ME4 %s1", "ME3 %s1", "ME2 %s1", "SMALL Lo%s"]
          let segmentsShort40 =
                map (\s -> printf s metric2)
                ["ME5 %s2", "ME4 %s2", "ME3 %s2", "ME2 %s4", "ME1 %s2"]
          facEWLong <- retsFromFile (printf "French/%s25_Portfolios_Size_%s_Equal_Wt.csv" region metric') $ equally (take numSizes segments)
          facEWShort <- retsFromFile (printf "French/%s25_Portfolios_Size_%s_Equal_Wt.csv" region metric') $ equally (take numSizes segmentsShort)
          facVWLong <- retsFromFile (printf "French/%s25_Portfolios_Size_%s_Value_Wt.csv" region metric') $ equally $ take numSizes segments ++ take numSizes segments40
          facVWShort <- retsFromFile (printf "French/%s25_Portfolios_Size_%s_Value_Wt.csv" region metric') $ equally $ take numSizes segmentsShort ++ take numSizes segmentsShort40


          let facEW = facEWLong - (if includeShorts then facEWShort else biasAdjustment)
          let facVW = facVWLong - (if includeShorts then facVWShort else 0)

          let starter = startingPeriod 1963 7
          myPrinter includeShorts ff3 metric
            (starter facEW) facVW
