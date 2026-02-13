{- |
Module      : GlobalMom
Description : Run a global momentum strategy, similar to Cambria's GMOM

Maintainer  : Michael Dickens
Created     : 2026-02-13

Code was originally written in 2020. I haven't tried to run it in a while.

-}

module GlobalMom where

import FFFactors
import Returns


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
