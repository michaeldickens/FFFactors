{- |
Module      : ReturnsHistory
Description : Generic type for a history of asset returns for which summary
              stats can be calculated.

Maintainer  : Michael Dickens
Created     : 2026-02-02

-}

module ReturnsHistory
  ( ReturnsHistory(..)
  ) where

import Period

import Data.Function (on)
import Data.Hashable
import Data.List (sortBy)
import qualified Data.HashMap.Strict as Map
import Text.Printf


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

  -- | Call a function operates over a list of `Double`s, and then
  -- convert it back to a `ReturnsHistory` with the same periods as the original
  -- input (if applicable).
  --
  -- Example: `rebuild drawdowns retSeries` calls `drawdowns retSeries` (which
  -- returns a `[Double]`) and then itself returns a `ReturnsHistory` with the
  -- same keys as `retSeries`.
  apply :: ([Double] -> [Double]) -- ^ Function to call
        -> a                      -- ^ ReturnHistory that will be converted to
                                  -- [Double]
        -> a                      -- ^ Result after applying and converting back

  -- | Like `apply`, except that if the resulting list contains too few
  -- elements, rather than raising an error, the elements are merged right with
  -- the original ReturnsHistory (the earliest dates are dropped).
  applyR :: ([Double] -> [Double]) -- ^ Function to call
         -> a                      -- ^ ReturnHistory that will be converted to
                                   -- [Double]
         -> a                      -- ^ Result after applying and converting back


instance ReturnsHistory [Double] where
  returnsToPrices rets = reverse $ foldl (\ps r -> ((1 + r) * head ps):ps) [1] rets
  pricesToReturns prices = zipWith (\x y -> y / x - 1) prices (tail prices)
  normalizePrices prices = map (/ (head prices)) prices
  toList = id
  fixDates = id
  apply f xs =
    let ys = f xs
    in if length xs == length ys
       then ys
       else error $ printf "ReturnsHistory.apply: result list has the wrong number of elements (%d expected, %d found)" (length xs) (length ys)

  applyR f xs = f xs

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
  apply f xs = zip (map fst xs) $ apply f (map snd xs)
  applyR f xs =
    reverse $ zip (reverse $ map fst xs)
    $ reverse $ applyR f (map snd xs)


wrap1 :: ([(Period, Double)] -> [(Period, Double)]) -> Map.HashMap Period Double -> Map.HashMap Period Double
wrap1 f = Map.fromList . f . sortBy (compare `on` fst) . Map.toList


instance ReturnsHistory (Map.HashMap Period Double) where
  returnsToPrices = wrap1 returnsToPrices
  pricesToReturns = wrap1 pricesToReturns
  normalizePrices = wrap1 normalizePrices
  toList = map snd . sortBy (compare `on` fst) . Map.toList
  fixDates = fixDates'
  apply f = wrap1 (apply f)
  applyR f = wrap1 (applyR f)


wrap2 :: (Map.HashMap Period Double -> Map.HashMap Period Double) -> Map.HashMap Int Double -> Map.HashMap Int Double
wrap2 f = Map.mapKeys year . f . Map.mapKeys yearToPeriod


-- In a map where keys are Ints, treat the keys as years
instance ReturnsHistory (Map.HashMap Int Double) where
  returnsToPrices = wrap2 returnsToPrices
  pricesToReturns = wrap2 pricesToReturns
  normalizePrices = wrap2 normalizePrices
  toList = toList . Map.mapKeys yearToPeriod
  fixDates = fixDates'
  apply f = wrap2 (apply f)
  applyR f = wrap2 (applyR f)


-- | For a list of quote maps, modify each quote map to contain only those dates
-- that are present in every map.
fixDates' :: (Hashable k) => [Map.HashMap k a] -> [Map.HashMap k a]
fixDates' quoteMaps =
  let intersection = foldl1 Map.intersection quoteMaps
  in map (Map.filterWithKey (\k _ -> Map.member k intersection)) quoteMaps
