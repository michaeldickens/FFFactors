{- |
Module      : Drawdown
Description : Utilities for calculating drawdowns

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2017-01-16

TODO: This file is old and unnecessarily complicated, I think it could be implemented much more simply using `French.hs:rollingDrawdowns`.

-}

module Drawdown (worstDrawdowns) where

import Returns
import Tools

import Data.Function
import Data.List
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.Time.Calendar
import Text.Printf

-- | Takes prices divided up into time segments (ordered chronologically) and
-- merges them into a single price list. Normalizes prices so they change
-- continuously across segments.
joinSequentialPrices :: [[(Day, Double)]] -> [(Day, Double)]
joinSequentialPrices segmentedPrices =
  foldl1 (
    \combined prices ->
    let adj = snd (last combined) / snd (head prices)
    in combined ++ map (\(d, p) -> (d, adj * p)) prices
  ) $ dropWhile null segmentedPrices

-- | Combines parallel lists of prices. Assumes that the price lists all have
-- equal length and parallel entries all occur on the same day.
joinParallelPrices :: [Double] -> [[(Day, Double)]] -> [(Day, Double)]
joinParallelPrices weights datesAndPrices =
  let dates = map fst $ head datesAndPrices :: [Day]
      prices = map (map snd) datesAndPrices :: [[Double]]
      weightedPrices =
        map (\(wt, firstPrice, prcs) -> map (* (wt / firstPrice)) prcs)
        $ zip3 weights (map head prices) prices
      joinedPrices = map sum $ transpose weightedPrices
  in zip dates joinedPrices

-- | Makes a sorted list contain only unique elements.
uniqifyBy :: (a -> a -> Ordering) -> [a] -> [a]
uniqifyBy cmp = foldr (\x xs -> if not (null xs) && cmp x (head xs) == EQ
                              then xs
                              else x:xs) []

uniqify :: (Ord a) => [a] -> [a]
uniqify = uniqifyBy compare

-- | Removes overlapping date ranges. O(n^2).
uniqifyDateRanges :: (Ord i) => [(i, i, Double)] -> [(i, i, Double)]
uniqifyDateRanges = reverse . go []
  where go accum [] = accum
        go accum ((start1, end1, drawdown1):xs) =
          if any (\(start2, end2, drawdown2) ->
                  (start1 <= start2 && end1 >= start2) || (start2 <= start1 && end2 >= start1)) xs
          then go accum xs
          else go ((start1, end1, drawdown1):accum) xs


-- | Combines multiple sets of prices with some weighting and rebalancing
-- schedule.
--
-- Price does not change between the end of one rebalancing period and
-- the beginning of the next, even though market price does change; consider
-- this the effect of temporarily holding cash while rebalancing.
combinePrices :: Int -> [Double] -> [[(Day, Double)]] -> [(Day, Double)]
combinePrices weeksToRebalance weights priceLists =
  joinSequentialPrices $ reverse $ buildSequences [] priceLists
  where buildSequences combined priceLists
          | null (head priceLists) = combined
          | otherwise = buildSequences ((joinParallelPrices weights
                                         $ map (take weeksToRebalance) priceLists)
                                        : combined)
                        (map (drop weeksToRebalance) priceLists)


-- | Calculate max drawdown proportion for a portfolio. Multiply by
-- 100 to get percent drawdown.
--
-- retsMap: A map from date to return.
--
-- return: ordered list of worst drawdowns, where each entry contains
-- (index of peak, index of trough, drawdown).
worstDrawdowns :: (Ord i) => Map.HashMap i Double -> [(i, i, Double)]
worstDrawdowns retsMap =
  let sortedRets = sortBy (compare `on` fst) $ Map.toList retsMap
      prices = uncurry zip $ (\(k, v) -> (k, returnsToPrices v)) $ unzip sortedRets
  in worstDrawdownsForPrices prices


-- | Calculate max drawdown proportion for a portfolio. Multiply by
-- 100 to get percent drawdown.
--
-- prices: a list of (index, price) pairs, ordered by index. index can be a Day
-- or a Period or whatever.
worstDrawdownsForPrices :: (Ord i) => [(i, Double)] -> [(i, i, Double)]
worstDrawdownsForPrices prices =
  let comparePrice f x y = if f (snd x) (snd y) then x else y
      maxPrice = comparePrice (>)
      minPrice = comparePrice (<)

  -- O(n) dynamic programming algorithm. Find the highest peak at each
  -- point moving left to right and the lowest trough moving right to
  -- left. The point with the largest difference between left-peak and
  -- right-trough gives the maximum drawdown.
      peaks = reverse $ fst $ foldl (\(xs, m) x -> (maxPrice x m : xs, maxPrice x m)) ([], head prices) prices
      troughs = fst $ foldr (\x (xs, m) -> (minPrice x m : xs, minPrice x m)) ([], last prices) prices
      drawdowns = zipWith (\(peakDay, peak) (troughDay, trough) ->
                            (peakDay, troughDay, (peak - trough) / peak))
                  peaks troughs

  -- Do a regular uniqify first because uniqifyDateRanges is O(n^2)
  in reverse $ uniqifyDateRanges $ uniqify
     $ sortBy (compare `on` (\(_, _, p) -> p)) drawdowns
