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
import Statistics.Regression
import System.IO
import Text.Printf

-- type Quote = Maybe Double
-- type QuoteSlice = Map.HashMap Text.Text Quote
-- type QuoteMap = Map.HashMap Period QuoteSlice

-- data Period = Period { year :: Int
--                      , month :: Int
--                      } deriving (Eq, Ord, Show, Hashable)

-- type RetSeries = Map.HashMap Period Double

tsmomPositions :: Double -> TrendRule -> Int -> RetSeries ->
                  QuoteMap -> RetSeries
tsmomPositions volScale trendRule lookbackPeriods rf quotes =
  let segments = Map.keys $ snd $ head $ Map.toList quotes

      histories = map (subtract rf . flip getRets1Skip quotes . Text.unpack) segments
      trendedHistories =
        map (longShortTrend' True volScale trendRule lookbackPeriods 0)
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


-- | Compute per-period (monthly) turnover for a portfolio segment.
-- Turnover is defined as the average absolute change in position weight
-- from one period to the next. For each asset, the position weight is
-- +/- 1/N (equal weight, long or short based on trend signal).
--
-- We approximate turnover by looking at the change in per-asset trended
-- return contributions between consecutive periods. A simpler and more
-- robust approach: turnover = mean |w_t - w_{t-1}| summed across assets.
--
-- Here we compute turnover as the fraction of the portfolio that is traded
-- each period, by tracking the signed positions.
tsmomTurnover :: Double -> TrendRule -> Int -> RetSeries ->
                 QuoteMap -> RetSeries
tsmomTurnover volScale trendRule lookbackPeriods rf quotes =
  let segments = Map.keys $ snd $ head $ Map.toList quotes

      histories = map (subtract rf . flip getRets1Skip quotes . Text.unpack) segments
      trendedHistories =
        map (longShortTrend' True volScale trendRule lookbackPeriods 0)
        histories

      periods =
        sort
        $ filter (\k -> any (Map.member k) trendedHistories)
        $ Map.keys quotes

      -- For each period, compute the vector of position weights.
      -- Each asset gets weight = (trended return sign) / N, where N is the
      -- number of tradable assets in that period.
      positionWeights period =
        let vals = map (Map.lookup period) trendedHistories
            tradable = catMaybes vals
            n = fromIntegral (length tradable)
        in  map (\mv -> case mv of
                   Nothing -> 0
                   Just v  -> v / n
                ) vals


      -- Turnover between two periods = sum of |w_t - w_{t-1}| / 2
      -- (dividing by 2 because a buy and sell are both counted)
      turnoverPairs = zipWith
        (\p1 p2 ->
          let w1 = positionWeights p1
              w2 = positionWeights p2
              to = sum (zipWith (\a b -> abs (a - b)) w1 w2) / 2
          in (p2, to)
        )
        periods (drop 1 periods)

  in Map.fromList turnoverPairs



data AssetClass = Equity | Bond | Commodity | Currency
  deriving (Eq, Ord, Show, Enum, Bounded)

assetClassPrefix :: AssetClass -> Text.Text
assetClassPrefix Equity    = Text.pack "Equity_"
assetClassPrefix Bond      = Text.pack "Bond_"
assetClassPrefix Commodity = Text.pack "Commodity_"
assetClassPrefix Currency  = Text.pack "Currency_"

-- | One-way transaction cost in basis points for each asset class.
assetClassCostBps :: AssetClass -> Double
assetClassCostBps Equity    = 6
assetClassCostBps Bond      = 1
assetClassCostBps Commodity = 10
assetClassCostBps Currency  = 3

-- | Filter a QuoteMap to only include columns whose name starts with the
-- given asset class prefix.
filterByAssetClass :: AssetClass -> QuoteMap -> QuoteMap
filterByAssetClass ac =
  Map.map (Map.filterWithKey (\k _ -> assetClassPrefix ac `Text.isPrefixOf` k))

-- | Count the number of assets in a QuoteMap (from any single period's slice).
numAssets :: QuoteMap -> Int
numAssets qm = case Map.elems qm of
  []    -> 0
  (s:_) -> Map.size s


-- | Mean of a RetSeries.
retSeriesMean :: RetSeries -> Double
retSeriesMean rs
  | Map.null rs = 0
  | otherwise   = sum (Map.elems rs) / fromIntegral (Map.size rs)

main :: IO ()
main = do
  rf <- loadRF
  assets <- loadDB "HLWZ/Asset_Returns.csv"

  let assetClasses = [Equity, Bond, Commodity, Currency]

  -- Split assets by class
  let assetsByClass = map (\ac -> (ac, filterByAssetClass ac assets)) assetClasses
  let totalLen = fromIntegral $ Map.size assets :: Double

  -- Calculate positions and turnover for each asset class
  let results = flip map assetsByClass $ \(ac, classAssets) ->
        let targetVol = 40 * (fromIntegral $ Map.size classAssets) / totalLen
            turnover = tsmomTurnover targetVol SMA 12 rf classAssets
        in (ac, turnover)

  -- Print per-class turnover statistics
  putStrLn "=== Per-Asset-Class Monthly Turnover ==="
  for_ results $ \(ac, turnover) -> do
    let meanTO = retSeriesMean turnover
    printf "  %-12s: mean monthly turnover = %.4f (%.1f%% annualized)\n"
      (show ac) meanTO (meanTO * 12 * 100)

  let totalAnnualTC = sum $ flip map results $ \(ac, turnover) ->
        let meanMonthlyTO = retSeriesMean turnover
            oneWayCost    = assetClassCostBps ac / 10000.0
            -- Annual two-way cost for this segment
            annualTC      = meanMonthlyTO * 12 * 2 * oneWayCost
        in  annualTC

  putStrLn ""
  printf "=== Total Annualized Transaction Cost (combined portfolio) ===\n"
  printf "  %.2f bps\n" (totalAnnualTC * 10000)
  printf "  %.4f%%\n" (totalAnnualTC * 100)
