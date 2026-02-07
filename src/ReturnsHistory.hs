{- |
Module      : ReturnsHistory
Description : Generic type for a history of asset returns for which summary
              stats can be calculated.

Maintainer  : Michael Dickens
Created     : 2026-02-02

-}

module ReturnsHistory
  ( ReturnsHistory
    -- * Returns/Prices conversion
    , returnsToPrices
    , pricesToReturns
    , normalizePrices
    , toList
    , fixDates
  ) where

import Period

import Data.Function (on)
import Data.Hashable
import Data.List (sortBy)
import qualified Data.HashMap.Strict as Map


class ReturnsHistory a where
  -- | Take an ordered list of returns and give the corresponding prices, assuming
  -- the starting price is 1.
  returnsToPrices :: a -> a

  pricesToReturns :: a -> a

  -- | Normalize a list of prices such that the first price is $1.
  normalizePrices :: a -> a

  toList :: a -> [Double]

  -- | If this ReturnsHistory type has Periods, pare down each history to
  -- include only the intersection of dates, i.e. dates that occur in all given
  -- histories.
  fixDates :: [a] -> [a]


instance ReturnsHistory [Double] where
  returnsToPrices rets = reverse $ foldl (\ps r -> ((1 + r) * head ps):ps) [1] rets
  pricesToReturns prices = zipWith (\x y -> y / x - 1) prices (tail prices)
  normalizePrices prices = map (/ (head prices)) prices
  toList = id
  fixDates = id


-- | Periods are assumed to be in sorted order.
instance ReturnsHistory [(Period, Double)] where
  -- | Length must be at least 2 in order to infer the interval between periods.
  -- Will raise an error if length is less than 2.
  returnsToPrices pairs =
    let periodSpacing = fst (pairs !! 1) `periodDiff` fst (pairs !! 0)
    in zip ([backwardMonths periodSpacing $ fst $ head pairs] ++ map fst pairs)
       (returnsToPrices $ map snd pairs)
  pricesToReturns pairs = zipWith (\(p, x) (_, y) -> (p, y / x - 1)) pairs (tail pairs)
  normalizePrices pairs = zip (map fst pairs) (normalizePrices $ map snd pairs)
  toList = map snd
  fixDates = map Map.toList . fixDates . map Map.fromList


instance ReturnsHistory (Map.HashMap Period Double) where
  returnsToPrices = Map.fromList . returnsToPrices . sortBy (compare `on` fst) . Map.toList
  pricesToReturns = Map.fromList . pricesToReturns . sortBy (compare `on` fst) . Map.toList
  normalizePrices = Map.fromList . normalizePrices . sortBy (compare `on` fst) . Map.toList
  toList = map snd . sortBy (compare `on` fst) . Map.toList
  fixDates = fixDates'


-- In a map where keys are Ints, treat the keys as years
instance ReturnsHistory (Map.HashMap Int Double) where
  returnsToPrices = Map.mapKeys year . returnsToPrices . Map.mapKeys yearToPeriod
  pricesToReturns = Map.mapKeys year . pricesToReturns . Map.mapKeys yearToPeriod
  normalizePrices = Map.mapKeys year . normalizePrices . Map.mapKeys yearToPeriod
  toList = toList . Map.mapKeys yearToPeriod
  fixDates = fixDates'


-- | For a list of quote maps, modify each quote map to contain only those dates
-- that are present in every map.
fixDates' :: (Hashable k) => [Map.HashMap k a] -> [Map.HashMap k a]
fixDates' quoteMaps =
  let intersection = foldl1 Map.intersection quoteMaps
  in map (Map.filterWithKey (\k _ -> Map.member k intersection)) quoteMaps
