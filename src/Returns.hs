{-# LANGUAGE RankNTypes #-}

{- |
Module      : Returns
Description : Calculation and manipulation of lists of returns

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2018-05-31

-}

module Returns
    ( -- * Type class
      ReturnsHistory(..)
      -- * Basic statistics
    , average
    , averageOrZero
    , totalReturn
    , geometricMean
    , stdev
    , variance
    , annualizedReturn
    , annualizedStdev
    , stderror
    , correlation
    , covariance
    , skewness
    , kurtosis
      -- * Risk metrics
    , downsideDeviation
    , downsideDeviation'
    , drawdowns
    , ulcerIndex
    , ulcerCovariance
    , ulcerCorrelation
    , worstDrawdowns
      -- * Regression
    , linearRegression
    , printLinearRegression
    , multipleRegression
    , likelihoodRatio
    , fTest
    , pValue
    , prettyPrintLikelihood
    , prettyPrintPValue
      -- * Other utilities
    , longShortReturns
    , kYearPeriods
    , kYearReturns
    , negativeKYearPeriods
    , underperformingKYearPeriods
    , returnsToAnnual
    , normalcdf
    ) where

import ReturnsHistory
import Period

import Data.Function (on)
import qualified Data.HashMap.Strict as Map
import Data.List
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as Vec
import qualified Numeric.GSL.Statistics as Stats
import qualified Statistics.Distribution as Dist
import Statistics.Distribution.StudentT (studentT)
import Statistics.Distribution.FDistribution (fDistribution)
import Text.Printf


-- | Convert a list of ReturnsHistory values into a list of lists. If the
-- histories have dates, take the intersection of the dates.
toLists :: ReturnsHistory a => [a] -> [[Double]]
toLists = map toList . fixDates


toVector :: ReturnsHistory a => a -> Vec.Vector Double
toVector = Vec.fromList . toList


toVectors :: ReturnsHistory a => a -> a -> (Vec.Vector Double, Vec.Vector Double)
toVectors xs ys = (Vec.fromList xl, Vec.fromList yl)
  where (xl:yl:[]) = toLists [xs, ys]


-- | Find the total return from a history of returns.
totalReturn :: ReturnsHistory a => a -> Double
totalReturn rets = (product $ map (\x -> max 0 $ x + 1) retsL) - 1
  where retsL = toList rets


average :: ReturnsHistory a => a -> Double
average = Stats.mean . toVector


-- | Average, but if there are zero elements, return 0.
averageOrZero :: ReturnsHistory a => a -> Double
averageOrZero xs'
  | Vec.size xs == 0 = 0
  | otherwise = Stats.mean xs
  where xs = toVector xs'


-- | Find the geometric average return. Calculate by summing in log space
-- because multiplying in linear space can cause overflow.
geometricMean :: ReturnsHistory a => a -> Double
geometricMean rets = (exp $ Stats.mean $ log $ 1 + toVector rets) - 1


stdev :: ReturnsHistory a => a -> Double
stdev xs = Stats.stddev $ toVector xs


variance :: ReturnsHistory a => a -> Double
variance xs = Stats.variance $ toVector xs


annualizedReturn :: ReturnsHistory a => a -> Double
annualizedReturn retsMap = (1 + geometricMean retsMap)**12 - 1


annualizedStdev :: ReturnsHistory a => a -> Double
annualizedStdev retsMap = sqrt 12 * stdev retsMap


-- | Note: `stderr` is the name of the IO output file so I can't use that name
stderror :: ReturnsHistory a => a -> Double
stderror xs = Stats.stddev vec / sqrt (fromIntegral $ Vec.size vec)
  where vec = toVector xs


downsideDeviation' :: ReturnsHistory a => Double -> a -> Double
downsideDeviation' minAcceptableReturn' rets =
  Stats.stddev $ Vec.fromList $ filter (< minAcceptableReturn') $ toList rets


downsideDeviation :: ReturnsHistory a => a -> Double
downsideDeviation rets = downsideDeviation' 0 rets


-- | Given a return series, get a list of the magnitudes of drawdown at each
-- point in time (or zero if the current price is a new peak).
drawdowns :: (ReturnsHistory a) => a -> [Double]
drawdowns rets =
  let prices = toList $ returnsToPrices rets
      runningMax = scanl1 max prices
      dds = zipWith (\x y -> x / y - 1) prices runningMax
  in tail dds


-- | Ulcer index measures the frequency and severity of drawdowns.
-- See http://www.tangotools.com/ui/ui.htm
ulcerIndex :: (ReturnsHistory a) => a -> Double
ulcerIndex rets =
  sqrt $ average $ map (**2) $ drawdowns rets


-- | Find all drawdowns for a portfolio and return them in sorted order from
-- deepest to shallowest. Drawdowns are returned as (key of peak, key of trough,
-- magnitude [decimal, not percent]).
worstDrawdowns :: (Ord i)
               => Map.HashMap i Double -- ^ Returns history.
               -> [(i, i, Double)]
worstDrawdowns rets =
  let sortedRets = sortBy (compare `on` fst) $ Map.toList rets
      pricesK = uncurry zip $ (\(k, v) -> (k, returnsToPrices v)) $ unzip sortedRets
      runningMaxK = scanl1 (\x y -> if snd x > snd y then x else y) pricesK
      dds = zipWith (
        \(peakK, peak) (currK, curr) -> ((peakK, peak), (currK, curr / peak - 1))
        ) runningMaxK pricesK

      -- list of lists ((peak key, peak), (DD key, DD))
      groups = groupBy ((==) `on` fst) dds

      -- list of lists ((peak key, peak), (trough key, trough))
      groupTroughs = map (minimumBy (compare `on` (snd . snd))) groups

      sorted = sortBy (compare `on` (snd . snd)) groupTroughs

  in map (\((peakKey, _), (ddKey, dd)) -> (peakKey, ddKey, dd)) sorted



-- | My attempt at a covariance equivalent for the ulcer index. Measures the
-- extent to which the drawdowns of two return series coincide.
ulcerCovariance :: (ReturnsHistory a) => a -> a -> Double
ulcerCovariance xs ys =
  let ddX = drawdowns xs
      ddY = drawdowns ys
  in if length ddX /= length ddY
     then error "ulcerCovariance: lengths of return series do not match"
     else average $ zipWith (*) ddX ddY


ulcerCorrelation :: (ReturnsHistory a) => a -> a -> Double
ulcerCorrelation xs ys =
  ulcerCovariance xs ys / (ulcerIndex xs * ulcerIndex ys)


correlation :: (ReturnsHistory a) => a -> a -> Double
correlation xs ys = Stats.correlation xv yv
  where (xv, yv) = toVectors xs ys


covariance :: (ReturnsHistory a) => a -> a -> Double
covariance xs ys = Stats.covariance xv yv
  where (xv, yv) = toVectors xs ys


skewness :: ReturnsHistory a => a -> Double
skewness = Stats.skew . toVector


kurtosis :: ReturnsHistory a => a -> Double
kurtosis = Stats.kurtosis . toVector


longShortReturns :: Double -> Double -> [Double] -> [Double] -> [Double]
longShortReturns longLeverage shortLeverage long short =
  map (\(l, s) -> l * longLeverage - s * shortLeverage) $ zip long short


kYearPeriods :: Int -> [Double] -> [[Double]]
kYearPeriods k rets =
  let firstK = take k rets
  in if length firstK == k
     then firstK : kYearPeriods k (tail rets)
     else []


kYearReturns :: Int -> [Double] -> [Double]
kYearReturns k rets = map totalReturn $ kYearPeriods k rets


negativeKYearPeriods :: Int -> [Double] -> [[Double]]
negativeKYearPeriods k rets =
  filter ((< 0) . totalReturn) $ kYearPeriods k rets


underperformingKYearPeriods :: Int -> [Double] -> [Double] -> [[Double]]
underperformingKYearPeriods k market rets =
  map snd $
  filter (\(m, r) -> totalReturn r < totalReturn m) $
  zip (kYearPeriods k market) (kYearPeriods k rets)


-- Compute a rolling statistic for every n periods.
--
-- TODO: finish. need to make `rebuild` a ReturnsHistory func
rolling :: (ReturnsHistory a)
        => ([Double] -> Double)
        -> Int
        -> a
        -> a
rolling func numPeriods rets = rets  -- TODO


-- | Converts a list of potentially sub-annual returns into annual returns, where
-- `interval` is the number of periods in each return.
returnsToAnnual :: Int -> [Double] -> [Double]
returnsToAnnual interval rets
  | monthsPerYear `mod` interval /= 0 =
    error $ printf "returnsToAnnual: interval %d must divide evenly into %d" interval monthsPerYear
  | otherwise =
    let bundleSize = monthsPerYear `div` interval
    in reverse $ go bundleSize [] rets
  where go :: Int -> [Double] -> [Double] -> [Double]
        go _          bundled [] = bundled
        go bundleSize bundled rs =
          go bundleSize
          ((totalReturn $ take bundleSize rs):bundled)
          (drop bundleSize rs)


-- | Standard normal CDF. Adapted from https://www.johndcook.com/blog/haskell-phi/
normalcdf :: (Floating a, Ord a) => a -> a
normalcdf x = y
    where
        a1 =  0.254829592
        a2 = -0.284496736
        a3 =  1.421413741
        a4 = -1.453152027
        a5 =  1.061405429
        p  =  0.3275911

        -- Abramowitz and Stegun formula 7.1.26
        sign = if x > 0
                   then  1
                   else -1
        t = 1.0/(1.0 + p * abs x / sqrt 2.0)
        e = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x/2.0)
        y = 0.5*(sign*e + 1.0)


-- | Params
-- - xs: list of independent variables
-- - ys: list of dependent variables
--
-- Return: ([intercept, slope], [intercept t-stat, slope t-stat], r^2)
--
-- Null hypothesis is that the slope is zero.
linearRegression :: [Double] -> [Double] -> ([Double], [Double], Double)
linearRegression xs ys =
  let xs' = map (:[]) xs
      (intercept:slope:[], tstats) = multipleRegression xs' ys
      predicted = map (\x -> intercept + slope * x) xs
      r = correlation predicted ys
  in if length xs == 0
     then error "linearRegression: cannot run regression on empty list"
     else if length xs /= length ys
          then error "linearRegression: lengths of xs and ys must match"
          else ([intercept, slope], tstats, r**2)


printLinearRegression :: [Double] -> [Double] -> IO ()
printLinearRegression xs ys = do
  let ([a, b], tstats, rsqr) = linearRegression xs ys
      tstat = tstats !! 1  -- t-stat of slope
      df = length ys - 2
      pval = pValue tstat df
  printf (printf "y = %s + %s * x (r^2 = %s, slope t-stat = %.2f, p = %s)\n"
         (if abs a < 0.01 then "%.1e" else "%.3f" :: String)
         (if abs b < 0.01 then "%.1e" else "%.3f" :: String)
         (if abs rsqr < 0.01 then "%.1e" else "%.2f" :: String)
         tstat
         (prettyPrintPValue pval)
         ) a b rsqr


-- | Params
-- - xs: MxN list of lists of independent variables
-- - ys: Mx1 list of dependent variables
--
-- Return: ([coefficients], [t-stats])
multipleRegression :: [[Double]] -> [Double] -> ([Double], [Double])
multipleRegression xs ys =
  let xsMatrix = LA.fromLists $ map (1:) xs
      ysMatrix = LA.col ys  -- construct a single-column matrix
      ysVec = LA.fromList ys
      coefs = head $ LA.toColumns $ LA.linearSolveLS xsMatrix ysMatrix

      residuals = ysVec - (xsMatrix LA.#> coefs)
      var = Stats.variance residuals

      -- Formula for stderrs:
      -- stderr vector = sqrt(residual var * (X^T * X)^-1)
      -- See https://stats.stackexchange.com/questions/173271/what-exactly-is-the-standard-error-of-the-intercept-in-multiple-regression-analy
      -- and https://stats.stackexchange.com/questions/27916/standard-errors-for-multiple-regression-coefficients
      stderrs = map sqrt $ LA.toList $ LA.takeDiag
                $ LA.scale var $ LA.inv
                $ (LA.tr xsMatrix) LA.<> xsMatrix

  in (LA.toList coefs, zipWith (/) (LA.toList coefs) stderrs)


-- | Get the likelihood ratio given a t-statistic and degrees of freedom. The likelihood ratio of observation x is defined as P(X = x | mu = x) / P(X = x | mu = 0).
likelihoodRatio :: Double -> Int -> Double
likelihoodRatio tstat df =
  let dist = studentT (fromIntegral df)
  in Dist.density dist 0 / Dist.density dist tstat


-- | Get the p-value given a t-statistic and degrees of freedom.
pValue :: Double -> Int -> Double
pValue tstat df =
  let dist = studentT (fromIntegral df)
  -- complCumulative = 1 - cumulative but with better float precision
  in 2 * (Dist.complCumulative dist $ abs tstat)


-- | Perform an F-test comparing two sample variances. Return the F-statistic
-- and p-value.
fTest :: (ReturnsHistory a)
      => a  -- ^ sample 1
      -> a  -- ^ sample 2
      -> (Double, Double) -- ^ (F-statistic, p-value)
fTest xs ys =
  let xList = toList xs
      yList = toList ys
      df1 = length xList - 1
      df2 = length yList - 1
      var1 = variance xList
      var2 = variance yList
      fstat = (max var1 var2) / (min var1 var2)
      dist = fDistribution df1 df2
      pval = 2 * (Dist.complCumulative dist $ abs fstat)
  in (fstat, pval)


-- | Convert a likelihood ratio to a string in a readable format, where the
-- format changes based on the size of the number.
prettyPrintLikelihood :: Double -> String
prettyPrintLikelihood lik
  | lik < 2 = printf "%.3f" lik
  | lik < 10 = printf "%.2f" lik
  | lik < 100 = printf "%.1f" lik
  | lik < 1e6 = printf "%.0f" lik
  | otherwise = printf "%.1e" lik


-- | Convert a p-value to a string in a readable format, where the format
-- changes based on the size of the number.
prettyPrintPValue :: Double -> String
prettyPrintPValue pval
  | pval < 1e-3 = printf "%.1e" pval
  | pval < 0.1 = printf "%.3f" pval
  | otherwise = printf "%.2f" pval
