{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module      : Quote
Description : Basic utilities for Ken French quotes

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2018-10-13

-}

module Quote where

import MaybeArithmetic()
import Read
import Returns
import Period

import Control.Monad (join)
import Data.Function (on)
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
import Debug.Trace
import Text.Printf


{- | Constants -}


-- | The longest date range that any data series might have. This is used to
-- create RetSeries from numeric literals.
longestDateRange :: [Period]
longestDateRange = [Period 1000 1..Period 2999 12]


countryCodes :: [String]
countryCodes = words "AUS BEL CHE DEU DNK ESP FIN FRA GBR ITA JPN NLD NOR PRT SWE USA"


{- | Data Definitions -}



type Quote = Maybe Double
type QuoteSlice = Map.HashMap Text.Text Quote
type QuoteMap = Map.HashMap Period QuoteSlice

type RetSeries = Map.HashMap Period Double
type PriceSeries = RetSeries


-- | `RetSeries` supports arithmetic operations. Applying an arithmetic
-- operation to two `RetSeries` is equivalent to applying that operation to each
-- pair of values for a key. If a key only exists on one `RetSeries`, it is
-- dropped.
--
-- `fromInteger` and `fromRational` create a `RetSeries` where every plausible
-- period has that particular value as its return. This is most useful for
-- multiplying `RetSeries` by constants, for example `2 * myRetSeries`
-- multiplies every value of `myRetSeries` by 2.
instance Num RetSeries where
  fromInteger x = Map.fromList $ zip longestDateRange
    $ repeat $ fromInteger x
  (+) = Map.intersectionWith (+)
  (*) = Map.intersectionWith (*)
  abs = Map.map abs
  negate = Map.map negate
  signum = Map.map signum

instance Fractional RetSeries where
  fromRational x = Map.fromList $ zip longestDateRange
    $ repeat $ fromRational x
  (/) = Map.intersectionWith (/)


{- | General-purpose functions -}


for :: [a] -> (a -> b) -> [b]
for xs f = map f xs


imputeReturn :: Text.Text -> Maybe Text.Text
imputeReturn t =
  let s = Text.unpack t
  in if s `elem` ["-99.99", "-99.9900", "-999", "--", ""]
     then Nothing
     else Just t


toOrderedList :: (Ord k) => Map.HashMap k v -> [v]
toOrderedList mapp = map snd $ sortBy (compare `on` fst) $ Map.toList mapp


percentToReturn :: Double -> Double
percentToReturn pct = pct / 100


traceShowSelf :: (Show a) => a -> a
traceShowSelf a = traceShow a a


{- | File I/O and data structure construction functions -}


-- | Read in a database from the given file path, returning a list of lists. The
-- first list is the header line containing the names of all the fields.
readDB :: Char -> FilePath -> IO [[Text.Text]]
readDB separator filename =
  Text.readFile filename >>=
  (return . map (map Text.strip . Text.split (==separator)) . Text.lines)


readCSVDB = readDB ','


makeQuoteSlice :: FilePath -> Int -> [Text.Text] -> [Text.Text]
                     -> (Period, QuoteSlice)
makeQuoteSlice _ _ [] (_:_) =
  error "makeQuoteSlice: header does not contain a date"
makeQuoteSlice _ _ (_:_) [] =
  error "makeQuoteSlice: line does not contain a date entry"
makeQuoteSlice _ _ [] [] =
  error "makeQuoteSlice: header and line are both empty"
makeQuoteSlice filename lineNum (dateFormatT:header) (date:rets) =
  -- date format is determined by the column header, wow I'm so clever
  -- this is the sort of crap I come up with at 10pm on a Friday
  let dateFormatS = Text.unpack dateFormatT
      dateFormat = if dateFormatS == "" then "%Y%m" else dateFormatS
      parsedDate =
        case parseTimeM True defaultTimeLocale dateFormat
             $ Text.unpack $ Text.strip date of
          Just d -> d
          Nothing -> error $ printf "File %s, line %d: Could not parse string as date: %s" filename lineNum (show date)
      (yr', mo, _) = toGregorian parsedDate
      yr = fromInteger yr'
      slice = Map.fromList
        $ map (\(h, r) -> (h, imputeReturn r >>= readDouble >>= return . percentToReturn))
        $ zip header rets
  in ((Period yr mo), slice)


fieldsToMap :: FilePath -> [[Text.Text]] -> QuoteMap
fieldsToMap _ [] =
  error ("fieldsToMap: rows are empty. there must be at least a header row")
fieldsToMap filename (header:fields) =
  Map.fromList $ zipWith (
  \lineNum row -> makeQuoteSlice filename lineNum header row
  ) [1..] fields


-- | Convert daily price series to a map from Periods to monthly returns. The
-- prices do not have to be daily, there just has to be at least one price per
-- month.
dailyPricesToMonthlyReturns :: FilePath -> [[Text.Text]] -> QuoteMap
dailyPricesToMonthlyReturns _ [] =
  error ("dailyPricesToMonthlyReturns: rows are empty. there must " ++
         "be at least a header row")
dailyPricesToMonthlyReturns filename (header:rows) =
  let dateFormatS = Text.unpack $ head header
      dateFormat = if dateFormatS == "" then "%Y%m" else dateFormatS
      getMonth date = let (_, mo, _) = toGregorian date
                      in mo

      monthlyPricePairs :: [(Period, [Double])]
      monthlyPricePairs =
        map (\(date, prices) ->
              let (yr, mo, _) = toGregorian date
              -- Add a one-month lag to match how Ken French and AQR do it: the
              -- returns labeled as "2020-02" are for the month from 2020-01-01
              -- to 2020-02-01.
              in (forwardMonths 1 $ Period (fromInteger yr) mo, prices)
            )
        -- Group by month and take the last price in each month.
        $ map (last . sortBy (compare `on` fst))
        $ groupBy ((==) `on` (getMonth . fst))
        $ zipWith (
        \lineNum (dateStr:priceStrs) ->
          let date = case parseTimeM True defaultTimeLocale dateFormat
                          $ Text.unpack dateStr of
                Just d -> d
                Nothing -> error $ printf "File %s, line %d: Could not parse string as date: %s" filename lineNum (show date)

              prices = map readDoubleUnsafe priceStrs
          in (date, prices)
        ) [(1::Int)..] rows

      periods = map fst monthlyPricePairs
      monthlyPriceList = map snd monthlyPricePairs

      monthlyRetsList =
        transpose
        $ map pricesToReturns
        $ transpose monthlyPriceList

      monthlyRets =
        Map.fromList
        $ map (\(period, rets) ->
                 (period, Map.fromList $ zip (tail header) $ map Just rets)
              ) $ zip periods monthlyRetsList

  in monthlyRets


-- | Load a quote map from a CSV file. The file should contain dates in the
-- first column and segment names in the header row. Each entry should be a
-- monthly return for a segment, represented as a percentage. In other words,
-- the file format should match the format of Ken French Data Library CSV files.
loadDB :: FilePath     -- ^ CSV file name within the resources/ directory under
                       -- the project root.
       -> IO QuoteMap
loadDB filename = readCSVDB ("resources/" ++ filename) >>= return . fieldsToMap filename


-- | Load a quote map from a CSV file. Like `loadDB` except that the file
-- contents are treated as prices, not returns. The prices are then converted to
-- returns.
loadPriceDB :: FilePath -> IO QuoteMap
loadPriceDB = loadDailyPriceDB


-- | Load a CSV file containing daily prices and convert them to monthly returns.
loadDailyPriceDB :: FilePath -> IO QuoteMap
loadDailyPriceDB filename = readCSVDB ("resources/" ++ filename) >>= return . dailyPricesToMonthlyReturns filename


-- | Return a pair of (minimum date, maximum date) for a `HashMap` where the keys
-- have type `Period`.
minMaxDates :: (Ord k) => Map.HashMap k a -> (k, k)
minMaxDates quoteMap =
  let periods = Map.keys quoteMap
  in foldl (\(minDate, maxDate) period -> (min period minDate, max period maxDate))
     (head periods, head periods)
     (tail periods)


-- | Return a list containing every date among keys in a `HashMap`, in order.
getDateRange :: (Enum k, Ord k) => Map.HashMap k a -> [k]
getDateRange quoteMap =
  let (min', max') = minMaxDates quoteMap
  in [min'..max']


-- | Filter a map with `Period` keys to only include entries that occur on or
-- after the given date (inclusive).
startingPeriod :: Int                  -- ^ Year
               -> Int                  -- ^ Month
               -> Map.HashMap Period a -- ^ The map to filter
               -> Map.HashMap Period a
startingPeriod yr mo myMap = Map.filterWithKey (\p _ -> p >= Period yr mo) myMap


-- | Filter a map with `Period` keys to only include entries that occur
-- before the given date (exclusive).
beforePeriod :: Int                  -- ^ Year
             -> Int                  -- ^ Month
             -> Map.HashMap Period a -- ^ The map to filter
             -> Map.HashMap Period a
beforePeriod yr mo myMap = Map.filterWithKey (\p _ -> p < Period yr mo) myMap


-- | Filter a map with `Period` keys to only include entries that occur
-- up to the given date (inclusive), but no later.
endingPeriod :: Int                  -- ^ Year
             -> Int                  -- ^ Month
             -> Map.HashMap Period a -- ^ The map to filter
             -> Map.HashMap Period a
endingPeriod yr mo myMap = Map.filterWithKey (\p _ -> p <= Period yr mo) myMap


-- | Look up a quote, returning Nothing if it can't be found.
lookupQuote :: Period -> Text.Text -> QuoteMap -> Quote
lookupQuote period key quoteMap =
  join $ Map.lookup period quoteMap >>= Map.lookup key


-- | Print more useful error messages in case period not found.
getQuote :: Period -> Text.Text -> QuoteMap -> Quote
getQuote period segmentName quoteMap =
  case Map.lookup period quoteMap of
    Nothing -> error $ printf "getQuote: %s not found" $ show period
    Just slice -> case Map.lookup segmentName slice of
      Nothing -> error $ printf "getQuote: Segment %s not found for %s" (show segmentName) (show period)
      Just quote -> quote


-- | Extract a map from periods to quotes (representing a particular column).
getSegment :: String -> QuoteMap -> Map.HashMap Period Quote
getSegment segmentName quoteMap =
  Map.map (\slice -> case Map.lookup (Text.pack segmentName) slice of
              Nothing -> error $ printf "getSegment: Segment %s not found" $ show segmentName
              Just quote -> quote) quoteMap


-- | For a data series with only annual returns, impute the return for the other
-- 11 months to 0%.
fillOutAnnualDataSeries :: QuoteMap -> QuoteMap
fillOutAnnualDataSeries quoteMap =
  let (minDate, maxDate) = minMaxDates quoteMap
      -- Extend min and max dates to cover an integral number of years. This
      -- avoids an issue where e.g. a date range 1990-01 to 1991-01 gets treated
      -- at 13 months, when really it should be 24 months.
      extendedMin = Period (year minDate) 1
      extendedMax = Period (year maxDate) 12
      dateRange = [extendedMin..extendedMax]

      sampleSlice = snd $ head $ Map.toList quoteMap
  in Map.fromList
     $ map (\period ->
              (period,
               if Map.member period quoteMap
               then quoteMap Map.! period
               else Map.map (\_ -> 0) sampleSlice
              )
           ) dateRange


class MonthlyToAnnual a where
  -- | For a data series with monthly returns, squish them down into annual returns.
  monthlyToAnnual :: a -> a


instance MonthlyToAnnual QuoteMap where
  monthlyToAnnual quoteMap =
    let (minDate, maxDate) = minMaxDates quoteMap
        -- Drop any months at the beginning or end of the date range that aren't part of a full year
        trueMin = head $ dropWhile (\p -> month p /= 1) [minDate..]
        trueMax = head $ dropWhile (\p -> month p /= 12) $ reverse $ [minDate..maxDate]

    in Map.fromList $ map (
      \yr ->
        let slices = map snd $ filter (\(period, _) -> year period == yr) $ Map.toList quoteMap
        in (Period yr 1,
            foldl1 (Map.intersectionWith (
                        \r1 r2 -> ((1 + r1) * (1 + r2)) - 1
                          )) slices
            )
      ) [(year trueMin)..(year trueMax)]


instance MonthlyToAnnual (RetSeries) where
  monthlyToAnnual rets =
    Map.map (\r -> r - 1)
    $ Map.foldlWithKey (
        \agg k r ->
          let y = Period (year k) defaultMonth
          in Map.insert y ((1 + r) * (fromMaybe 1 $ Map.lookup y agg)) agg
        ) Map.empty rets

{-

Data manipulations

-}


-- | For a list of quote maps, find the longest date range where the ends are present in all quote maps.
jointDateRange :: [Map.HashMap Period a] -> [Period]
jointDateRange quoteMaps =
  let datePairs = map minMaxDates quoteMaps
      (minDate, maxDate) = foldl1 (\(min1, max1) (min2, max2) ->
                                     (max min1 min2, min max1 max2)) datePairs
  in [minDate..maxDate]


-- | For a list of quote maps, find all dates that are present in all quote maps.
jointDates :: [Map.HashMap Period a] -> [Period]
jointDates quoteMaps =
  getDateRange $ foldl1 Map.intersection quoteMaps


mergeQuoteSlices :: QuoteSlice -> QuoteSlice -> QuoteSlice
mergeQuoteSlices slice1 slice2 =
  Map.unionWithKey (\segment _ _ -> error $ printf "mergeQuoteSlices: Segment %s appears in both slices" $ show segment) slice1 slice2


mergeQuoteMaps :: QuoteMap -> QuoteMap -> QuoteMap
mergeQuoteMaps qm1 qm2 =
  Map.fromList
  $ map (\p -> (p, case liftA2 mergeQuoteSlices (Map.lookup p qm1) (Map.lookup p qm2) of
                     Just x -> x
                     Nothing -> error $ printf "mergeQuoteMaps: %s not present" $ show p
               ))
  $ jointDates [qm1, qm2]


-- | Attach a prefix to each quote map's segments to avoid name collisions, then merge the quote maps.
mergeQuoteMapsWithNameCollisions :: [(String, QuoteMap)] -> QuoteMap
mergeQuoteMapsWithNameCollisions quoteMaps =
  let renamedQuoteMaps =
        map (\(prefix, qm) -> Map.map (
                \slice ->
                  Map.fromList
                  $ map (\(k, r) ->
                            let newKey = Text.append (Text.pack prefix) k
                            in (newKey, r)
                        ) $ Map.toList slice
                ) qm
            ) quoteMaps
  in foldl1 mergeQuoteMaps renamedQuoteMaps


writeToFile :: FilePath -> [(String, RetSeries)] -> IO ()
writeToFile filename' namedHistories =
  let filename =  "resources/" ++ filename'
      seriesNames = map (Text.pack . fst) namedHistories
      histories = map snd namedHistories
      periods = getDateRange $ foldl1 Map.union histories
      header = Text.intercalate "," ("%Y-%m" : seriesNames)
      rows = map (makeRow histories) periods
  in Text.writeFile filename $ Text.unlines (header : rows)
  where
    makeRow :: [RetSeries] -> Period -> Text.Text
    makeRow series period =
        let periodText = Text.pack $ show period
            values = map (lookupValue period) series
        in Text.intercalate "," (periodText : values)

    lookupValue :: Period -> RetSeries -> Text.Text
    lookupValue period m = case Map.lookup period m of
        Just v  -> Text.pack $ printf "%.6f" $ 100 * v
        Nothing -> ""
