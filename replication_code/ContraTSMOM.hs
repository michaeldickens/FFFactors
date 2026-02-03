{- |
Module      : ContraTSMOM
Description : Replication code for "Contra 'TSMOM: Is It There?'"

Maintainer  : Michael Dickens
Created     : 2026-02-02

I made significant refactors to French while writing the post, so a lot of the
code I wrote won't work anymore. This file doesn't contain the actual code I
used; instead it has some illustrative examples for how to re-create my work.

-}

module Main where

import French


main = do
  regressOnTSH


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
  mom <- retsFromFile1 "French/Momentum_Factor.csv" "Mom"
  trend <- retsFromFile1 "Global_Factor_Premiums.csv" "Trend^MA"
  rf <- loadRF

  putStrLn "\n=== Trend (Global Factor Premiums) Regression on FF4 ===\n"
  printFactorRegressionOrg trend rf
    [ getRets1 "Mkt-RF" ff3
    , getRets1 "SMB" ff3
    , getRets1 "HML" ff3
    , mom
    ]
    ["Mkt", "SMB", "HML", "Mom"]
