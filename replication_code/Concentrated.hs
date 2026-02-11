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


myPrinter :: QuoteMap -> String -> String -> RetSeries -> RetSeries -> IO ()
myPrinter ff3 metric title ys xs = do
  let rf = getRets1 "RF" ff3
  let beta = getRets1 "Mkt-RF" ff3
  let mkt = beta + rf
  let smb = getRets1 "SMB" ff3
  let df = (Map.size $ head $ fixDates [beta, xs, ys]) - 4
  let (alpha:coefs, tstat:_, rsqr) = factorRegression ys rf [beta, smb, xs - mkt]
  printf "| %s %s | %.2f | %.2f | %.2f | %.2f%%%s | %.2f |\n"
    metric
    title
    (coefs!!0)
    (coefs!!1)
    (coefs!!2)
    (100 * ((1 + alpha)**12 - 1))
    (starsForPval $ pValue tstat df)
    tstat


main = do
  for_ [("", "US"), ("Developed_ex_US_", "Developed ex-US")] $ \(region, regionName) -> do
    ff3 <- loadDB (printf "French/%s3_Factors.csv" region)

    printf "\n**** %s\n" regionName
    putStrLn "| | beta | SMB | factor | annual alpha | t-stat |"
    putStrLn "|-|"

    for_ [4, 5] $ \numSizes -> do
      -- as of 2025, $1B cutoff is top 8 deciles and $2B is top 7 deciles. see Ken
      -- French ME_Breakpoints (not downloaded)
      for_ [("B/M", "BM"), ("Profitability", "OP"), ("Investment", "INV"), ("Momentum", "PRIOR")] $ \(metric, metric2) -> do
        let metric' = map (\c -> if c == '/' then '-' else c) metric
        let segments =
              map (\s -> printf s metric2)
              ["BIG Hi%s", "ME4 %s5", "ME3 %s5", "ME2 %s5", "SMALL Hi%s"]
        let segments40 =
              map (\s -> printf s metric2)
              ["ME5 %s4", "ME4 %s4", "ME3 %s4", "ME2 %s4", "ME1 %s4"]
        valEW <- retsFromFile (printf "French/%s25_Portfolios_Size_%s_Equal_Wt.csv" region metric') $ equally (take numSizes segments)
        valVW <- retsFromFile (printf "French/%s25_Portfolios_Size_%s_Value_Wt.csv" region metric') $ equally $ take numSizes segments ++ take numSizes segments40

        myPrinter ff3 metric ("EW " ++ if numSizes == 4 then "(realistic)" else "(all-cap)") valEW valVW
