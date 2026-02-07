{- |
Module      : ContraTSMOM
Description : Replication code for "Contra 'TSMOM: Is It There?'"

Maintainer  : Michael Dickens
Created     : 2026-02-02

I made significant refactors to the main code base while writing the post, so a
lot of the code I wrote won't work anymore. This file doesn't contain most of
the code I used; instead it has some illustrative examples for how to re-create
my work.

I also did some regressions using Guofu Zhou's published replication data for
"Time Series Momentum: Is It There?"
(https://guofuzhou.github.io/zpublications.html), by modifying the code in
"Python Program"/Table9.py.

-}

module Main where

import FFFactors


-- | Regress TSMOM (based on HLWZ data) against TSH.
regressOnTSH = do
  rf <- loadRF
  strats <- loadDB "HLWZ/TSMOM_TSH.csv"

  -- As a "deeper" replication, I wrote code to reconstruct HLWZ's managed
  -- futures strategy from the individual asset returns. My results are not
  -- exactly the same but they're very close. I identified a couple of small
  -- methodological differences[1] but even correcting those, I didn't figure
  -- out how to get 100% alignment. Still, I got close enough that I don't think
  -- it matters. I could probably get 100% alignment by going through and
  -- finding the specific trades where my implementation and theirs disagree,
  -- but that sounds like a lot of work.
  --
  -- [1] An example of a difference is that they measured the sign of a trend by
  -- comparing the 12-month total return vs. the 12-month return of RF, whereas
  -- I measured it by comparing the 12-month excess return against zero.
  myTSMOM <- managedFutures TMOM 12

  let tsmom = getRets1 "TSMOM" strats
  let tsh = getRets1 "TSH" strats

  putStrLn "\n=== TSMOM Regression on TSH ===\n"
  printFactorRegressionOrg tsmom rf [tsh] ["TSH"]

  plotLineGraphLog "images/TSMOM-vs-TSH.png" "TSMOM vs. TSH" "Price"
    [ ("TSMOM", returnsToPrices $ getRets1 "TSMOM" strats)
    , ("TSH", returnsToPrices $ getRets1 "TSH" strats)
    ]

  putStrLn "\n=== Their TSMOM Regression on My TSMOM ===\n"
  printFactorRegression tsmom rf [myTSMOM] ["MyTSMOM"]


-- | Regress TSMOM (based on Global Factor Premiums data) against the FF US
-- four-factor model back to 1927.
regressOnFF4To1927 = do
  ff3 <- loadDB "French/3_Factors.csv"
  beta <- retsFromFile1 "French/3_Factors.csv" "Mkt-RF"
  mom <- retsFromFile1 "French/Momentum_Factor.csv" "Mom"
  trend <- retsFromFile1 "Global_Factor_Premiums.csv" "Trend^MA"
  rf <- loadRF

  let ff4 = [ getRets1 "Mkt-RF" ff3
            , getRets1 "SMB" ff3
            , getRets1 "HML" ff3
            , mom
            ]
  let ff4Names = ["beta", "size", "value", "momentum"]

  putStrLn "\n=== Trend Regression on US FF4, 1927–2016 ===\n"
  printFactorRegressionOrg trend 0 ff4 ff4Names

  gfp <- loadDB "Global_Factor_Premiums.csv"
  let trend = getRets1 "Trend^MA" gfp

  let names = ["Val^EQ", "Mom^EQ", "Val^MA", "Mom^MA"]
  let gfpFacs = map (flip getRets1 gfp) names

  putStrLn "\n=== Trend Regression on Global Factor Premiums, 1927–2016 ===\n"
  printFactorRegressionOrg trend 0 (beta:gfpFacs) ("beta":names)

  putStrLn "\n=== Trend Regression on US FF4 + GFP, 1927–2016 ===\n"
  printFactorRegressionOrg trend 0 (ff4 ++ gfpFacs) (ff4Names ++ names)


-- | Compare a regression of the live trend index vs. AQR's gross-of-costs
-- benchmark, regressed against US + Developed four-factor model.
regressLiveFunds = do
  rf <- loadRF
  factors <- sequence
    $ [ retsFromFile1 "French/3_Factors.csv" "Mkt-RF"
      , retsFromFile1 "French/3_Factors.csv" "SMB"
      , retsFromFile1 "French/3_Factors.csv" "HML"
      , retsFromFile1 "French/Momentum_Factor.csv" "Mom"
      , retsFromFile1 "French/3_Factors_Developed_ex_US.csv" "Mkt-RF"
      , retsFromFile1 "French/3_Factors_Developed_ex_US.csv" "SMB"
      , retsFromFile1 "French/3_Factors_Developed_ex_US.csv" "HML"
      , retsFromFile1 "French/Momentum_Factor_Developed_ex_US.csv" "WML"
      ]
  let names = ["beta^US", "size^US", "value^US", "momentum^US", "beta^Dev", "size^Dev", "value^Dev", "momentum^Dev"]

  live <- retsFromFile1 "Trend_Index.csv" "Trend Index"
  aqr <- retsFromFile1 "AQR/TSMOM.csv" "TSMOM"

  putStr "Trend Index = "
  printFactorRegression live rf factors names
  putStr "\nAQR TSMOM = "
  printFactorRegression aqr rf factors names


main = do
  regressLiveFunds
