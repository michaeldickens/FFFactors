{- |
Module      : GetRets
Description : Get returns from a QuoteMap

Maintainer  : Michael Dickens
Created     : 2026-02-10

-}

module GetRets
    ( getPeriodReturn
    , getRets
    , getRets1
    , getRetsSkip
    , getRets1Skip
    , getRetsStrict
    , getRets1Strict
    , imposeCost
    , liveFundRets
    , loadRF
    , unsafeGetRets
    , unsafeGetRets1
    , retsFromFile
    , retsFromFile1
  ) where


import Period
import Quote

import Control.Monad (when)
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.Printf


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


getPeriodReturn :: Period -> Text.Text -> QuoteMap -> Double
getPeriodReturn period segment quoteMap =
  case getQuote period segment quoteMap of
    Nothing -> error $ printf "Segment %s at period %s is missing a return" (show segment) (show period)
    Just ret -> ret


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
-- beginning of the series is reached. If any periods are missing in the middle,
-- raise an error.
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


-- | Get weighted returns for segments. Skip any missing periods at the end,
-- then take all returns going back in time until there's another missing period
-- or the beginning of the series is reached. If any periods are missing in the
-- middle, raise an error.
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
--
-- Live funds must be stored in resources/Live_Funds and the yield-adjusted
-- price column must be named "adj_close".
liveFundRets :: String -> IO (RetSeries)
liveFundRets ticker =
  loadPriceDB ("Live_Funds/" ++ ticker ++ ".csv") >>= return . getRets1 "adj_close"
