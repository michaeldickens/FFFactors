{-# OPTIONS_GHC
    -fno-warn-unused-binds
#-}
{- |
Module      : French
Description :

Maintainer  : Michael Dickens
Created     : 2018-05-31

-}

module French
    ( (!)
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
      -- * Data manipulations
    , rebuild
    , imposeCost
    , jointReturns
    , growUlcer
    , imposePenaltyOnCAGR
    , conservative
    , conservative'
      -- * Date utilities
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
      -- * Regression
    , factorRegression
    , starsForPval
    , printFactorRegression
    , printFactorRegressionOrg
      -- * Trend/Momentum
    , trendOverlay
    , TrendRule(..)
    , tsMomentum
    , managedFutures
    , managedFutures'
    , timeSeriesHistory

    -- * -- From Quote.hs --

      -- * Utilities
    , for
    , toOrderedList
    , traceShowSelf
    -- * Types
    , Period(..)
    , QuoteKey
    , Quote
    , QuoteSlice
    , QuoteMap
    , RetSeries
    , PriceSeries
      -- * Loading data
    , loadDB
    , loadPriceDB
    , readCSVDB
      -- * Accessing quotes
    , getQuote
    , lookupQuote
    , getSegment
      -- * Date utilities
    , defaultMonth
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
      -- * File I/O
    , writeToFile

    -- * -- From ReturnsHistory.hs --
    , returnsToPrices
    , pricesToReturns
    , normalizePrices
    , fixDates

    -- * -- From ChartPlot.hs --
    , plotLineGraph
    , plotLineGraphLog

    ) where

import ChartPlot
import Period
import Quote
import Returns

import Control.Monad (when)
import Data.Function (on)
import Data.IORef
import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as V
import System.IO.Unsafe
import Text.Printf


{- |

Returns

-}


getPeriodReturn :: Period -> Text.Text -> QuoteMap -> Double
getPeriodReturn period segment quoteMap =
  case getQuote period segment quoteMap of
    Nothing -> error $ printf "Segment %s at period %s is missing a return" (show segment) (show period)
    Just ret -> ret


-- | Helper function. If `segment` is "*Annual Cost*", calculate the cost using
-- `weight`. Otherwise, return `value`.
--
-- Thanks to lazy evaluation, `value` may be an error in case segment is
-- "*Annual Cost*" and this will still work.
handleCost :: String -> Double -> Double -> Double
handleCost segment weight value
  | segment == "*Annual Cost*" =
    if weight > 0
    then error "Annual cost must be a negative number"
    else 1 - (1 - weight)**(1/12)
  | otherwise = value


-- | Get weighted returns for segments. If any periods are missing, raise an
-- error.
--
-- A segment name of "*Annual Cost*" will be treated as a fixed annual cost,
-- such as a management fee. Applied monthly such that it annualizes to the
-- given number. Cost is as a decimal, not a percent.
getRetsStrict :: [(String, Double)] -> QuoteMap -> RetSeries
getRetsStrict segmentWeights quoteMap =
  -- This maps over a List instead of a Map so that if any segments are missing,
  -- the first missing segment will also be the first chronologically.
  Map.fromList
  $ map
  (\period ->
    (period,
     sum $ for segmentWeights $ \(segment, weight) ->
        handleCost segment weight
        $ max (-1) $ weight * (getPeriodReturn period (Text.pack segment) quoteMap)))
  $ getDateRange quoteMap


-- | Get returns for segments. If any segments have missing periods, instead of
-- raising an error, simply exclude the missing periods.
unsafeGetRets :: [(String, Double)] -> QuoteMap -> RetSeries
unsafeGetRets segmentWeights quoteMap =
  Map.map fromJust $ Map.filter isJust $ flip Map.mapWithKey quoteMap
  $ \period _ ->
    sum $ for segmentWeights $ \(segment, weight) ->
      case getQuote period (Text.pack segment) quoteMap of
        Nothing -> Nothing
        Just ret ->
          Just $ handleCost segment weight $ max (-1) $ weight * ret


-- | Get returns for segment. Skip any missing periods at the end, then take all
-- returns going back in time until there's another missing period or the
-- beginning of the series is reached.
getRets1Skip :: String -> QuoteMap -> RetSeries
getRets1Skip segment quoteMap =
  let getter p = getQuote p (Text.pack segment) quoteMap
      periods =
        Set.fromList
        $ takeWhile (isJust . getter)
        $ dropWhile (isNothing . getter)
        $ reverse
        $ getDateRange quoteMap
      skipQM = Map.filterWithKey (\p _ -> p `Set.member` periods) quoteMap
  in getRets1Strict segment skipQM


-- | Get weighted returns for segments, skipping any missing periods at the
-- start or end.
--
-- The special segment name "*Annual Cost*" will be treated as a fixed annual
-- cost, such as a management fee. Applied monthly such that it annualizes to
-- the given number. Cost is as a decimal, not a percent.
getRetsSkip :: [(String, Double)]  -- ^ A list of (segment name, weight).
            -> QuoteMap            -- ^ Map of asset quotes.
            -> RetSeries           -- ^ Weighted returns of the provided
                                   -- segments.
getRetsSkip segmentWeights quoteMap =
  sum $ map (
    \(segment, wt) ->
      if segment == "*Annual Cost*"
      then imposeCost (-wt) 0
      else Map.map (
        \x -> wt * x
        ) $ getRets1Skip segment quoteMap
  ) segmentWeights


getRets1Strict :: String -> QuoteMap -> RetSeries
getRets1Strict segmentName = getRetsStrict [(segmentName, 1)]


-- | An alias for `getRetsSkip`.
getRets = getRetsSkip

-- | An alias for `getRets1Skip`.
getRets1 = getRets1Skip


unsafeGetRets1 segmentName = unsafeGetRets [(segmentName, 1)]


-- | Read a return series directly from a file. This is equivalent to calling `loadDB` and then `getRets`.
retsFromFile :: FilePath            -- ^ Filename.
             -> [(String, Double)]  -- ^ List of (segment name, weight).
             -> IO (RetSeries)
retsFromFile filename segments = do
  quotes <- loadDB filename

  -- Check that the segments exist in the file. Checking here rather than
  -- just-in-time makes it possible to see which file contains the error
  let slice = head $ Map.elems quotes
  sequence $ for segments $ \(name, _) ->
    when (isNothing $ Map.lookup (Text.pack name) slice) $
      error $ printf "retsFromFile: In %s, segment %s not found" filename name

  return $ getRets segments quotes


retsFromFile1 :: FilePath -> String -> IO (RetSeries)
retsFromFile1 filename segment = retsFromFile filename [(segment, 1)]


-- | Load the risk-free rate.
loadRF :: IO (RetSeries)
loadRF = retsFromFile1 "French/3_Factors.csv" "RF"


-- | Load returns for a live fund. This function takes a ticker instead of a filename.
liveFundRets :: String -> IO (RetSeries)
liveFundRets ticker =
  loadPriceDB ("Live_Funds/" ++ ticker ++ ".csv") >>= return . getRets1 "adj_close"


{- |

Helpers

-}


returnsWithFilter :: (Period -> QuoteKey -> Bool) -> QuoteMap -> [Double]
returnsWithFilter f quoteMap =
  for (getDateRange quoteMap) $ \period ->
    average $ map (\segment -> getPeriodReturn period segment quoteMap)
    $ filter (f period) $ Map.keys $ quoteMap ! period


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


-- | Call a function that converts a `RetSeries` to `[Double]`, and then convert
-- it back to a `RetSeries` with the same keys as the original input.
--
-- Example: `rebuild drawdowns retSeries` calls `drawdowns
-- retSeries` (which returns a `[Double]`) and then itself returns a `RetSeries`
-- with the same keys as `retSeries`.
rebuild :: (RetSeries -> [Double]) -> RetSeries -> RetSeries
rebuild f rets =
  Map.fromList $ zip (getDateRange rets) (f rets)


-- | Impose an annual cost on a return series by subtracting a fixed amount from
-- every period's return. `imposeCost` can be used to simulate a management fee
-- or similar.
imposeCost :: Double     -- ^ Annual cost to impose (as a decimal, not a
                         -- percent).
           -> RetSeries  -- ^ Return series on which to impose a cost.
           -> RetSeries
imposeCost annualCost rets
  | annualCost < 0 = error "imposeCost: Cost cannot be negative."
  | annualCost >= 1 = error "imposeCost: Cost must be less than 1."
  | otherwise =
    let monthlyCost = (1 + annualCost)**(1/12) - 1
    in Map.map (\x -> x - monthlyCost) rets


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
-- This function preserves CAGR but does not preserve any arithmetic moments
-- (mean, stdev, skew, etc.).
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


-- | Same as Map.!, except with better error messaging.
(!) :: (Show k, Hashable k) => Map.HashMap k a -> k -> a
(!) m k = case Map.lookup k m of
  Just x -> x
  Nothing -> error $ "cannot find key: " ++ show k


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
  -- calculated. we just want to go long in those cases, so we do Map.union
  Map.union (
  -- having the overlay as 0% short/100% short is equivalent to a long/short
  -- trend scaled down to 50%, plus a 50% static short position
  rets
  - 0.5 * (index - rf)
  + 0.5 * longShortTrend 0 trendRule lookbackPeriods 0 (index - rf)
  ) rets


-- | Apply a trend rule to a single asset.
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
  utilityWithConsumptionV minNumPeriods discountRate consumptionRate utility
  $ V.fromList $ toList rets


-- | Compute total utility over time with period consumption. Where
-- `utilityWithConsumption` takes a map from periods to returns,
-- `utilityWithConsumptionV` takes an ordered vector of returns.
utilityWithConsumptionV :: Int -> Double -> Double -> (Double -> Double)
                        -> V.Vector Double -> Double
utilityWithConsumptionV minNumPeriods discountRate consumptionRate utility rets
  | consumptionRate >= 1 = -(1/0)  -- -infinity
  | otherwise =
      case V.foldl (\(netWorth, totalUtil, t) r ->
                     (netWorth * (1 - consumptionRate) * (1 + r),
                       safeAdd totalUtil ((exp (-t * discountRate)) * (utility $ netWorth * consumptionRate)),
                       t + 1
                     ))
           (1, 0, 0)
           $ V.generate (max minNumPeriods (V.length rets)) (\i -> rets V.! (i `mod` V.length rets))
      of (_, util, _) -> util
  where safeAdd x y = if x == -(1/0) || y == -(1/0)  -- -(1/0) == -infinity
                      then -(1/0)
                      else x + y


{- |

Regressions

-}


-- | Extract factors from a quote map. `retSeries` determines which periods to
-- include.
extractFactors :: RetSeries -> QuoteMap -> [String] -> [RetSeries]
extractFactors retSeries factorData simpleFactors =
  map (
  \segment -> foldl (
    \accum period -> let ret = getPeriodReturn period (Text.pack segment) factorData
                     in Map.insert period ret accum
    ) Map.empty $ Map.keys retSeries
  ) simpleFactors


-- | Regress `retSeries - rf` over `factors`.
--
-- `rf` is the risk-free rate, which will be subtracted from `retSeries`. If the
-- risk-free rate should not be subtracted, pass in a 0 literal.
--
-- `factors` is a list of return series, each representing a factor.
--
-- If `retSeries` and `factors` cover different periods, then the regression
-- will only include periods where all necessary data is present. Periods with
-- partial data will be skipped.
--
-- Return a tuple of
-- - list including first the intercept, then the slope coefficients
-- - list of t-stats first for the intercept, then for the slope coefficients
-- - r^2 between predicted returns (according to the regression) and
--   actual returns
factorRegression :: RetSeries                    -- ^ Dependent variable
                 -> RetSeries                    -- ^ Risk-free rate
                 -> [RetSeries]                  -- ^ List of factors (independent variables)
                 -> ([Double], [Double], Double) -- ^ (coefficients, t-stats, r^2)
factorRegression retSeries rf factors =
  let -- Include only the periods where retSeries, rf, and all factors have data
      periods = sort $ foldl (
        \accum factor -> intersect accum (Map.keys factor)
        ) (Map.keys $ retSeries + rf) factors

      -- Construct the independent variable from factor returns
      xs = map (\period -> map (\rets ->
                                  rets!period
                               ) factors) periods

      -- Construct the dependent variable from the return series
      ys = map (\period -> retSeries!period - rf!period) periods

      (intercept:coefs, tstats) = case multipleRegression xs ys of
        (i:c, ts) -> (i:c, ts)
        (wrong, _) -> error $ "multipleRegression failed, returned these coefs: " ++ show wrong

      predictedReturns = map (sum . zipWith (*) coefs) xs
      corr = correlation (map (retSeries !) periods) predictedReturns

  in (intercept:coefs, tstats, corr**2)


-- | Print a number of asterisks based on the given p-value. *, **, and ***
-- indicate p < 0.05, p < 0.01, and p < 0.001 respectively.
starsForPval :: Double -> String
starsForPval pval
  | pval < 0.001 = "***"
  | pval < 0.01 = "**"
  | pval < 0.05 = "*"
  | otherwise = ""


-- | Print a factor regression as an equation.
--
-- >>> printFactorRegression myPortfolio rf [mkt - rf, smb, hml] ["Mkt", "SMB", "HML"]
-- 1.12*Mkt + 0.34*SMB + 0.93*HML
--	(r² = 0.96, alpha = -1.00%, ℒ = 6)

printFactorRegression :: RetSeries   -- ^ Dependent variable
                      -> RetSeries   -- ^ Risk-free rate
                      -> [RetSeries] -- ^ List of factors (independent variables)
                      -> [String]    -- ^ List of names of independent
                                     -- variables (for printing)
                      -> IO ()
printFactorRegression retSeries rf factors factorNames =
  let (intercept:coefs, tstat:_, rsqr) = factorRegression retSeries rf factors
      df = Map.size retSeries - length (intercept:coefs)
      pval = pValue tstat df
      likelihood = likelihoodRatio tstat df
      stars = starsForPval pval
  in do
    putStr
      $ foldl (
      \accum (name, coef) ->
        if accum == ""
        then accum ++ printf "%5.2f*%s" coef name
        else accum ++ printf " %s %.2f*%3s"
             (if coef < 0 then "-" else "+" :: String) (abs coef) name
      )
      ("" :: String)
      $ zip factorNames coefs
    printf "\n\t(r² = %.2f, alpha = %5.2f%%%s, t-stat = %.2f, ℒ = %s)\n"
      (rsqr)
      (100 * ((1 + intercept)**12 - 1))
      stars
      tstat
      (prettyPrintLikelihood likelihood)


-- | Print a factor regression as a Markdown or org-mode table.
--
-- The final parameter is a list of names to match up with the provided list of
-- factors.
printFactorRegressionOrg :: RetSeries   -- ^ Dependent variable
                         -> RetSeries   -- ^ Risk-free rate
                         -> [RetSeries] -- ^ List of factors (independent variables)
                         -> [String]    -- ^ List of names of independent variables
                                        -- (for printing)
                         -> IO ()
printFactorRegressionOrg retSeries rf factors factorNames = do
  let (coefs, tstats, rsqr) = factorRegression retSeries rf factors
  let df = Map.size retSeries - length factors
  putStrLn "|                  |       | t-stat | likelihood |"
  putStrLn "|------------------|-------|--------|------------|"
  let alphaName = "annual alpha"
  sequence $ for (zip3 (alphaName:factorNames) coefs tstats) $ \(name, coef', tstat) -> do
    let likelihood = likelihoodRatio tstat df
    let pval = pValue tstat df
    let stars = starsForPval pval
    let coef = if name == alphaName then 100 * ((1 + coef')**12 - 1) else coef'
    let percentSign = if name == alphaName then "%" else "" :: String
    printf "| %-16s | %5.2f%s%-3s | %.1f | %s |\n"
      name coef percentSign stars tstat (prettyPrintLikelihood likelihood)
  printf   "| r²               | %.2f  |        |            |\n" rsqr


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
