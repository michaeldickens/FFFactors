{-# OPTIONS_GHC
    -fno-warn-unused-binds
#-}
{- |
Module      : FFFactors
Description :

Maintainer  : Michael Dickens
Created     : 2018-05-31

-}

module FFFactors
    ( -- * Data manipulations
      imposeCost
    , jointReturns
    , growUlcer
    , imposePenaltyOnCAGR
    , conservative
    , conservative'
      -- * Statistics
    , annualizedReturn
    , annualizedStdev
    , printStats
    , printStats'
    , printStatsOrg
    , setRfToZero
    , setRfToNonzero
    , setRFToZero
    , setRFToNonzero
      -- * Trend/Momentum
    , trendOverlay
    , TrendRule(..)
    , tsMomentum
    , longShortTrend
    , longShortTrend'
    , managedFutures
    , managedFutures'
    , timeSeriesHistory

      -- * Regression
    , factorRegression
    , printFactorRegression
    , printFactorRegressionOrg
    , starsForPval

    -- * Period
    , Period(..)
    , defaultMonth
    , yearToPeriod
    , forwardMonths
    , backwardMonths
    , periodToDay

      -- * Loading data
    , loadRF
      -- * Return calculations
    , getRets
    , getRets1
    , getRetsSkip
    , getRets1Skip
    , getRetsStrict
    , getRets1Strict
    , liveFundRets
    , unsafeGetRets
    , unsafeGetRets1
    , retsFromFile
    , retsFromFile1

       -- * Utilities
    , (!)
    , for
    , toOrderedList
    , traceShowSelf
    -- * Quote types
    , Quote
    , QuoteSlice
    , QuoteMap
    , RetSeries
    , PriceSeries
      -- * File I/O
    , loadDB
    , loadPriceDB
    , loadDailyPriceDB
    , readCSVDB
    , writeToFile
      -- * Accessing quotes
    , getQuote
    , lookupQuote
    , getSegment
      -- * Date utilities
    , getDateRange
    , minMaxDates
    , jointDateRange
    , jointDates
    , startingPeriod
    , beforePeriod
    , endingPeriod
      -- * Data manipulation
    , fillOutAnnualDataSeries
    , MonthlyToAnnual(..)
    , mergeQuoteMaps
    , mergeQuoteMapsWithNameCollisions

    -- * -- ReturnsHistory functions --
    , returnsToPrices
    , pricesToReturns
    , normalizePrices
    , fixDates

    -- * -- Chart plotting --
    , plotLineGraph
    , plotLineGraphLog

    ) where

import ChartPlot
import GetRets
import Period
import Quote
import Regression
import Returns

import Control.Monad (when)
import Data.Function (on)
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.IO.Unsafe
import Text.Printf


{- |

Returns

-}


{- |

Helpers

-}


-- | Take a list of (returns, weighting) pairs and calculate the combined return.
--
-- Result will only include periods where every given map has a return.
jointReturns :: [(RetSeries, Double)] -> RetSeries
jointReturns rets =
  -- compute join return for each period
  Map.fromList $ map (\period -> (period, sum $ map (\(rs, wt) -> wt * (rs ! period)) rets))
  -- get periods that exist in every map
  $ Set.toList $ foldl1 Set.intersection $ map (Set.fromList . Map.keys . fst) rets


{- |

Data manipulations

-}


-- | Basic implementation of Newton's Method for finding the root of a
-- differentiable function.
newtonsMethod :: Double -> (Double -> Double) -> (Double -> Double) -> Double
              -> Double
newtonsMethod tolerance f f' x0 = go 100 x0
  where go :: Int -> Double -> Double
        go 0 _ = error "newtonsMethod: Maximum iterations exceeded."
        go numIters x
          | abs (f x) <= tolerance = x
          | otherwise = go (numIters - 1) (x - f x / f' x)


-- | Impose a penalty on the CAGR (compound annual growth rate) of a return
-- series. Unlike `imposeCost`, this function guarantees that CAGR will decrease
-- by exactly the given amount.
--
-- This function preserves higher moments (standard deviation, skew, etc.) but
-- does not preserve arithmetic mean or ulcer index.
imposePenaltyOnCAGR :: Double    -- ^ Annual penalty to impose (as a decimal,
                                 -- not a percent).
                    -> RetSeries -- ^ Return series on which to impose a
                                 -- penalty.
                    -> RetSeries
imposePenaltyOnCAGR penalty rets
  | penalty < 0 = error "imposePenaltyOnCAGR: Penalty cannot be negative."
  | penalty >= 1 = error "imposePenaltyOnCAGR: Penalty must be less than 1."
  | otherwise =
    let retsL = toList rets
        cagr = annualizedReturn rets

        -- target monthly return after penalty
        target = (1 + cagr - penalty)**(1/12) - 1

        gmean x = geometricMean $ map (\r -> r - x) retsL
        func x = gmean x - target
        derivative x =
          (1 + gmean x)
          * (average $ map (\r -> (-1) / (1 + r - x)) retsL)

        guess = penalty / 12

        -- note: this converges to 0.0 in only ~3 iterations
        solution = newtonsMethod 1e-20 func derivative guess

    in Map.map (\r -> r - solution) rets


-- | Make a return series more conservative by multiplying the excess CAGR and
-- the ulcer index by the given multiplicative factors.
conservative' :: Double     -- ^ multiplicative factor for CAGR (should be < 1)
              -> Double     -- ^ multiplicative factor for ulcer index
              -> RetSeries  -- ^ risk-free rate (for calculating excess CAGR)
              -> RetSeries  -- ^ return series
              -> RetSeries  -- ^ modified return series
conservative' cagrMultiple ulcerMultiple rf rets =
  let excess = rets - rf
      cagr = annualizedReturn excess

      -- if CAGR is already negative, make it even more negative
      penalty = if cagr >= 0
                then (1 - cagrMultiple) * cagr
                else -cagr * (1 / cagrMultiple - 1)
      newExcess = imposePenaltyOnCAGR penalty excess
  in rf + growUlcer ulcerMultiple newExcess


-- | Make a return series more conservative by cutting the excess CAGR
-- in half and increasing ulcer index by 50%.
conservative :: RetSeries  -- ^ risk-free rate (for calculating excess CAGR)
             -> RetSeries  -- ^ return series
             -> RetSeries  -- ^ modified return series
conservative = conservative' 0.5 1.5


-- | Multiply a return series's ulcer index by the given scalar. A value of 1
-- indicates no change.
--
-- This function does not preserve any arithmetic moments (mean, stdev, skew,
-- etc.). If the end price is an all-time high, this function preserves CAGR.
-- Otherwise, this function decreases CAGR in proportion to the depth of the
-- drawdown as of the final price.
--
-- `growUlcer` works by scaling up the magnitude of every drawdown relative to
-- the previous peak price. Scaling every drawdown by the same factor also
-- scales the ulcer index by that factor, thanks to the identity
--
--     sqrt((k*x)^2 + (k*y)^2) = k*sqrt(x^2 + y^2)
growUlcer :: Double -> RetSeries -> RetSeries
growUlcer scalar rets
  | scalar < 0 = error $ "growUlcer: scalar must be positive, not " ++ show scalar
  | otherwise =
    let prices = toList $ returnsToPrices rets
        runningMax = scanl1 max prices
        dds = drawdowns rets

        -- We can reconstruct the price from runningMax and drawdown. We use that
        -- fact to make the drawdowns bigger.
        altPrices = zipWith (\p dd -> p * (1 + scalar * dd)) runningMax dds
    in Map.fromList $ zip (getDateRange rets) $ pricesToReturns altPrices


{- |

Strategies

-}


-- | Find the momentum of a return series, calculated as the last price minus
-- the simple moving average (SMA) over the prior `lookbackMonths` periods. If
-- there are not enough periods to calculate momentum, return `Nothing`.
smaMomentum :: Int -> Period -> RetSeries -> Maybe Double
smaMomentum lookbackMonths currPeriod retsMap = do
  let periods = [backwardMonths lookbackMonths currPeriod..backwardMonths 1 currPeriod]
  rets <- sequence $ map (\p -> Map.lookup p retsMap) periods
  let prices = returnsToPrices rets
  return $ last prices - average prices


-- | Find the time series momentum of a return series over the prior
-- `lookbackMonths` periods. If there are not enough periods to calculate
-- momentum, return `Nothing`.
tsMomentum :: Int -> Period -> RetSeries -> Maybe Double
tsMomentum lookbackMonths currPeriod retsMap =
  let periods = [backwardMonths lookbackMonths currPeriod..backwardMonths 1 currPeriod]
  in (sequence $ map (\p -> Map.lookup p retsMap) periods)
     >>= Just . totalReturn


-- | Find the time series momentum of a return series over the prior
-- `lookbackMonths` periods, skipping the most recent month. If there are not
-- enough periods to calculate momentum, return `Nothing`.
laggedTSMomentum :: Int -> Period -> RetSeries -> Maybe Double
laggedTSMomentum lookbackMonths currPeriod retsMap =
  let periods = [backwardMonths lookbackMonths currPeriod..backwardMonths 2 currPeriod]
  in (sequence $ map (\p -> Map.lookup p retsMap) periods)
     >>= Just . totalReturn


-- | Explicitly cheat and find whichever assets will have the strongest future return.
godStrategy :: Int -> QuoteMap -> Period -> Text.Text -> Maybe Double
godStrategy lookForwardMonths quoteMap currPeriod segment =
  let periods = map (\n -> forwardMonths n currPeriod) [0..(lookForwardMonths-1)]
  in (sequence $ map (\p -> lookupQuote p segment quoteMap) periods)
     >>= Just . totalReturn


data TrendRule =
  SMA          -- ^ Current price minus simple moving average of price, after
               -- subtracting RF
  | TMOM       -- ^ Total excess return over last N months
  | LaggedTMOM -- ^ Total excess return over last N months, skipping most recent
               -- month


trendFunc :: TrendRule
          -> Int -> Period -> RetSeries -> Maybe Double
trendFunc trendRule =
  case trendRule of
    SMA -> smaMomentum
    TMOM -> tsMomentum
    LaggedTMOM -> laggedTSMomentum


-- | Apply a trend overlay to a return series such that the portfolio will have
-- no exposure to the hedge during an uptrend, and 100% short exposure to the
-- hedge during a downtrend.
trendOverlay :: TrendRule -> Int -> RetSeries
             -> RetSeries
             -> RetSeries
             -> RetSeries
trendOverlay trendRule lookbackPeriods rf index rets =
  -- longShortTrend removes any periods that are too early for trend to be
  -- calculated. we just want to go long in those cases; Map.union adds those
  -- periods back in.
  Map.union (
  -- having the overlay as 0% short/100% short is equivalent to a long/short
  -- trend scaled down to 50%, plus a 50% static short position.
  rets
  - 0.5 * (index - rf)
  + 0.5 * longShortTrend 0 trendRule lookbackPeriods 0 (index - rf)
  ) rets


-- | Apply a trend rule to a single asset. The asset is assumed to be gross of
-- the risk-free rate.
--
-- positionOnly: If False, calculate return as normal. If True, do not calculate
-- return; instead only calculate the position size.
longShortTrend' :: Bool
                -> Double
                -> TrendRule
                -> Int
                -> RetSeries
                -> RetSeries
                -> RetSeries
longShortTrend' positionOnly volScale trendRule lookbackPeriods rf history =
  Map.fromList
  -- need at least 12 months to estimate volatility
  $ for (drop (max 12 lookbackPeriods)
         $ sortBy (compare `on` fst)
         $ Map.toList history
        )
  (\(period, currRet) ->
      let trendStrength =
            fromJust
            (trendFunc trendRule lookbackPeriods period (history - rf))
          yearRets =
            map (history!)
            [backwardMonths 12 period..backwardMonths 1 period]
          annualVol = sqrt 12 * (stdev yearRets)
          position =
            (
              if trendStrength >= 0
              then  1
              else -1
            ) * (
              if volScale > 0
              then volScale / 100 / annualVol
              else 1
            )
      in (period, position * (if positionOnly then 1 else currRet))
  )


-- | Apply a trend rule to a single asset. Go long when the asset has a positive
-- trend (above RF) and go short when it has a negative trend.
--
-- volScale: Target volatility (as a percentage) for each individual asset.
-- Recommended value is 40. If zero or negative, all weights are set to 1. When
-- combining multiple assets, the caller should divide position sizes by the
-- total number of assets to achieve an appropriate total volatility.
--
-- trendRule: One of SMA or TMOM.
--
-- lookbackPeriods: Number of periods to look back for the TSMOM rule.
longShortTrend :: Double
               -> TrendRule
               -> Int
               -> RetSeries
               -> RetSeries
               -> RetSeries
longShortTrend = longShortTrend' False


-- | Calculate returns for a full managed futures strategy that goes long or
-- short every security based on trend.
--
-- volScale: Target volatility (as a percentage) for each individual asset.
-- Recommended value is 40. If zero or negative, use equal-weighting instead of
-- vol-weighting.
--
-- trendRule: One of SMA or TMOM.
--
-- lookbackPeriods: Number of periods to look back for the TSMOM rule.
managedFutures' :: Double -> TrendRule -> Int -> RetSeries ->
                   QuoteMap -> RetSeries
managedFutures' volScale trendRule lookbackPeriods rf quotes =
  let segments = Map.keys $ snd $ head $ Map.toList quotes

      histories = map (subtract rf . flip getRets1Skip quotes . Text.unpack) segments
      trendedHistories =
        map (longShortTrend volScale trendRule lookbackPeriods 0)
        histories

      -- include only periods where at least one asset can be trended
      periods =
        filter (\k -> any (Map.member k) trendedHistories)
        $ Map.keys quotes

      portfolio =
        Map.fromList $ for periods (
        \k ->
          let rets =
                map fromJust $ filter isJust
                $ map (Map.lookup k) trendedHistories

              -- count how many segments are tradable in this period
              numHoldings = fromIntegral $ length rets
          in (k, rf!k + sum rets / numHoldings)
        )

  in portfolio


-- | Calculate returns for a full managed futures strategy that goes long or
-- short every security based on trend.
managedFutures :: TrendRule -- ^ Which trend rule to use. One of SMA | TMOM.
               -> Int       -- ^ Number of months' lookback for the trend signal.
               -> IO (RetSeries)
managedFutures trendRule lookbackPeriods = do
  rf <- loadRF
  assets <- loadDB "HLWZ/Asset_Returns.csv"
  return $ managedFutures' 40 trendRule lookbackPeriods rf assets


-- TODO delete me when you're done testing
timeSeriesHistory :: IO RetSeries
timeSeriesHistory = do
  rf <- loadRF
  quotes <- loadDB "HLWZ/Asset_Returns.csv"

  let segments = Map.keys $ snd $ head $ Map.toList quotes

      histories = map (subtract rf . flip getRets1Skip quotes . Text.unpack) segments
      trendedHistories =
        map (
        \hist ->
           Map.filter (/= (-99))
           $ Map.mapWithKey (
            \k r ->
              if k < Period 1986 1
              then -99
              -- HLWZ only uses most recent 500 months
              -- and it uses arithmetic mean, not geometric
              else if (average $ take 500 $ toList $ Map.filterWithKey (\k2 _ -> k2 < k) $ hist) >= 0
                   then r
                   else -r
            ) hist
        ) histories

      -- include only periods where at least one asset can be trended
      periods =
        filter (\k -> any (Map.member k) trendedHistories)
        $ Map.keys quotes

      portfolio =
        Map.fromList $ for periods (
        \k ->
          let rets =
                map fromJust $ filter isJust
                $ map (Map.lookup k) trendedHistories
              numHoldings = fromIntegral $ length rets
          in (k, rf!k + sum rets / numHoldings)
        )

  return portfolio


{- |

Leverage

-}


-- (Annual) cost of leverage on top of risk-free rate
excessCostOfLeverage :: Double
excessCostOfLeverage = 0.00


monthlyExcessCostOfLeverage :: Double
monthlyExcessCostOfLeverage = 1 - ((1 - excessCostOfLeverage)**(1/12))


-- | Produce a map from periods to returns for a portfolio consisting of the
-- given segments leveraged at the given level.
--
-- Leverage costs the risk-free rate. The input quote map must contain a symbol
-- "RF" giving the risk-free rate for each period.
returnsForLeveragedSegments :: [(String, Double)] -> Double -> QuoteMap -> RetSeries
returnsForLeveragedSegments segments leverage quoteMap =
  let gross = getRets ((map (\(seg, prop) -> (seg, prop * leverage)) segments)
                        ++ [("RF", 1 - leverage)]) quoteMap
  in Map.map (\r -> max (-1) $ r - (max 0 (leverage - 1)) * monthlyExcessCostOfLeverage) gross


-- | Utility function with constant relative risk aversion. rra=0 corresponds to
-- risk neutrality; rra=1 corresponds to logarithmic utility.
crraUtility :: Double -> Double -> Double
crraUtility 1 consumption   = log consumption
crraUtility rra consumption = (consumption**(1 - rra) - 1) / (1 - rra)


-- | Compute total utility over time with periodic consumption.
--
-- discountRate: The per-period rate at which future utility is discounted.
--
-- consumptionRate: How much of the portfolio to consume per period.
--
-- utility: A function from consumption to utility.
--
-- rets: A map from periods to returns.
--
-- minNumPeriods: If the return series has fewer than this many periods, repeat
-- it until it has this many. Over short periods, the optimizer will tend to
-- over-consume to try to end up with as little money as possible at the end.
-- You need about 1000 years to prevent over-consumption.
utilityWithConsumption :: Int -> Double -> Double -> (Double -> Double)
                       -> RetSeries -> Double
utilityWithConsumption minNumPeriods discountRate consumptionRate utility rets =
  utilityWithConsumptionL minNumPeriods discountRate consumptionRate utility
  $ toList rets


-- | Compute total utility over time with period consumption. Where
-- `utilityWithConsumption` takes a map from periods to returns,
-- `utilityWithConsumptionV` takes an ordered vector of returns.
utilityWithConsumptionL :: Int -> Double -> Double -> (Double -> Double)
                        -> [Double] -> Double
utilityWithConsumptionL minNumPeriods discountRate consumptionRate utility rets
  | consumptionRate >= 1 = -(1/0)  -- -infinity
  | otherwise =
      case foldl (\(netWorth, totalUtil, t) r ->
                     (netWorth * (1 - consumptionRate) * (1 + r),
                       safeAdd totalUtil ((exp (-t * discountRate)) * (utility $ netWorth * consumptionRate)),
                       t + 1
                     ))
           (1, 0, 0)
           $ map (\i -> rets !! (i `mod` length rets)) [0..max minNumPeriods (length rets) - 1]
      of (_, util, _) -> util
  where safeAdd x y = if x == -(1/0) || y == -(1/0)  -- -(1/0) == -infinity
                      then -(1/0)
                      else x + y


{- |

Summary stats

-}


-- | Technically, this isn't the Sharpe ratio because it uses geometric return
-- instead of arithmetic return.
sharpeRatio :: Double -> RetSeries -> RetSeries -> Double
sharpeRatio periodsPerYear riskFreeRates retsMap =
  let rfAdjusted = toList $ Map.intersectionWith (-) retsMap riskFreeRates
      adjRet = 12 * average rfAdjusted
      annStdev = stdev (toList retsMap) * sqrt periodsPerYear
  in adjRet / annStdev


-- | UPI = (arithmetic return - RF) / ulcerIndex
ulcerPerformanceIndex :: RetSeries -> RetSeries -> Double
ulcerPerformanceIndex riskFreeRates retsMap =
  let rfAdjusted = toList $ Map.intersectionWith (-) retsMap riskFreeRates
      adjRet = 12 * average rfAdjusted
      ulcer = ulcerIndex $ toList retsMap
  in adjRet / ulcer


-- | Given a map of monthly returns, find the annualized return for each decade
-- (including partial decades).
returnsByDecade :: RetSeries -> RetSeries
returnsByDecade retsMap =
  let bookendDecade f =
        (year $ f $ sort $ map fst $ Map.toList retsMap) `div` 10
      firstDecade = bookendDecade head
      lastDecade = 1 + bookendDecade last

      decades = map (\d -> Period (10*d) 1) [firstDecade..lastDecade]
      decadeRanges = zip decades (drop 1 decades)
  in Map.fromList $ map
     (\(start, end) -> (
         start,
         (\xs -> (1 + geometricMean xs)**12 - 1) $ toList $ Map.filterWithKey (\k _ -> k >= start && k < end) retsMap
         )) decadeRanges


-- NOINLINE is necessary to prevent unsafePerformIO from executing multiple
-- times. I don't think it matters here but I included it to be safe.
{-# NOINLINE summaryStatsRF #-}
summaryStatsRF :: IORef (RetSeries)
summaryStatsRF = unsafePerformIO $ do
  usQuotes <- loadDB "French/3_Factors.csv"
  newIORef $ getRets1 "RF" usQuotes


-- | Set the risk-free rate to zero. Use this before calling `printStats`
-- etc. on return series that already subtract out the risk-free rate.
setRfToZero :: IO ()
setRfToZero = do
  modifyIORef' summaryStatsRF (Map.map (const 0))


-- | Set the risk-free rate to the rate from the US 3-factor model. The
-- risk-free rate is nonzero by default, so you only need to call this if you've
-- previously called `setRfToZero`.
setRfToNonzero :: IO ()
setRfToNonzero = do
  usQuotes <- loadDB "French/3_Factors.csv"
  let nonzeroRF = getRets1 "RF" usQuotes
  writeIORef summaryStatsRF nonzeroRF


setRFToZero = setRfToZero
setRFToNonzero = setRfToNonzero


summaryStats :: RetSeries -> IO (Double, Double, Double, Double, Double, Double)
summaryStats retsMap = do
  -- TODO: This doesn't work for RORE because rf only goes back to 1926
  rf <- readIORef summaryStatsRF
  let annualizedRet = annualizedReturn retsMap
  let annStdev = stdev retsMap * sqrt 12

  return (sharpeRatio 12 rf retsMap
         , ulcerPerformanceIndex rf retsMap
         , 100 * annualizedRet
         , 100 * annStdev
         , skewness retsMap
         , 100 * ulcerIndex retsMap)


-- | Same as printStats, but don't print drawdowns.
printStats' :: RetSeries -> IO ()
printStats' retsMap = do
  (sharpe, upi, ret, stdev', skew, ulcer) <- summaryStats retsMap
  printf "Sharpe  : %4.2f\n" sharpe
  printf "UPI     : %4.2f\n" upi
  printf "CAGR    : %4.1f\n" ret
  printf "Stdev   : %4.1f\n" stdev'
  printf "Skewness: %4.1f\n" skew
  printf "Ulcer   : %4.1f\n" ulcer
  putStrLn ""


-- | Given a list of monthly returns, print summary statistics and max drawdowns.
printStats :: RetSeries -> IO ()
printStats retsMap = do
  (sharpe, upi, cagr, stdev', skew, ulcer) <- summaryStats retsMap
  let firstPeriod = minimum $ Map.keys retsMap
  let lastPeriod = maximum $ Map.keys retsMap
  let dds = take 5 $ worstDrawdowns retsMap
  printf "Sharpe  : %4.2f\n" sharpe
  printf "UPI     : %4.2f\n" upi
  printf "CAGR    : %4.1f\n" cagr
  printf "Stdev   : %4.1f\n" stdev'
  printf "Skewness: %4.1f\n" skew
  printf "Ulcer   : %4.1f\n" ulcer
  printf "Max drawdowns (%s to %s): \n\t%s\n" (show firstPeriod) (show lastPeriod) $ intercalate "\n\t" $ map (\(p, t, dd) -> (printf "%.1f%% -> %d-%02d to %d-%02d" (100 * dd) (year p) (month p) (year t) (month t)) :: String) dds
  putStrLn ""


-- NOINLINE is necessary to prevent unsafePerformIO from executing multiple
-- times. I don't think it matters here but I included it to be safe.
{-# NOINLINE printedOrgBoilerplateYet #-}
printedOrgBoilerplateYet :: IORef Bool
printedOrgBoilerplateYet = unsafePerformIO $ newIORef False


printStatsOrg :: String -> RetSeries -> IO ()
printStatsOrg name retsMap = do
  -- Keep a global variable tracking whether org boilerplate has been printed
  -- yet. If it hasn't, print it now.
  boilerplateYet <- readIORef printedOrgBoilerplateYet
  when (not boilerplateYet) $ do
    writeIORef printedOrgBoilerplateYet True
    printOrgBoilerplate

  (sharpe, upi, cagr, stdev', _, ulcer) <- summaryStats retsMap
  printf "| %-15s | %6.2f | %4.2f | %4.1f | %5.1f | %5.1f |\n" name sharpe upi cagr stdev' ulcer


printOrgBoilerplate :: IO ()
printOrgBoilerplate = do
  putStrLn "|                 | Sharpe |  UPI | CAGR | Stdev | Ulcer |"
  putStrLn "|-----------------|--------|------|------|-------|-------|"


printROREStatsOrg :: String -> RetSeries -> IO ()
printROREStatsOrg name retsMap = do
  quoteMap <- loadDB "RORE/countries/USA.csv"
  let rf = getRets1 "bill_rate" quoteMap
  let retsList = toList retsMap
  let sharpe = sharpeRatio 1 rf retsMap
  let cagr = 100 * geometricMean retsList
  let stdev' = 100 * stdev retsList
  let ulcer = 100 * ulcerIndex retsList

  printf "| %-10s | %6.2f | %4.1f | %5.1f | %5.1f |\n" name sharpe cagr stdev' ulcer


printROREOrgBoilerplate :: IO ()
printROREOrgBoilerplate = do
  putStrLn "|            | Sharpe | CAGR | Stdev | Ulcer |"
  putStrLn "|------------|--------|------|-------|-------|"
