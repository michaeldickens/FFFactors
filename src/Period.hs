{- |
Module      : Period
Description : Date type used by French module

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-09-07

Define the Period type that includes a year and a month. Using
Period gives more compile-time safety than using the types from
Data.Time.Calendar.

-}

module Period
    ( -- * Period type
      Period(..)
    , monthsPerYear
    , defaultMonth
    , yearToPeriod
    , forwardMonths
    , backwardMonths
    , periodDiff
    , nextYear
    , periodToDay
    ) where

import Data.Hashable
import Data.Time.Calendar hiding (periodToDay)
import Text.Printf


{- |

General-purpose functions.

-}

{- |

Period

A pair of ints where the first is the year and the second is the
month. One-indexed, so the first month is 1 and the last month is 12.

-}

data Period = Period { year :: Int
                     , month :: Int
                     } deriving (Eq, Ord)

instance Enum Period where
  toEnum x = Period (x `div` 12) ((x `mod` 12) + 1)
  fromEnum period = 12 * year period + month period - 1

instance Hashable Period where
  hashWithSalt salt period = hashWithSalt salt $ fromEnum period

instance Show Period where
  show (Period yr mo) = printf "%04d-%02d" yr mo

monthsPerYear :: Int
monthsPerYear = 12

-- | Indicate which month to use for securities that only have one
-- month of data per year.
defaultMonth :: Int
defaultMonth = 12

yearToPeriod :: Int -> Period
yearToPeriod y = Period y defaultMonth

-- | Move a year/month pair forward N months.
forwardMonths :: Int -> Period -> Period
forwardMonths n = toEnum . (+ n) . fromEnum

-- | Move a year/month pair backward N months.
backwardMonths :: Int -> Period -> Period
backwardMonths n = toEnum . (\x -> x - n) . fromEnum

-- | Find the number of months between two periods.
periodDiff :: Period -> Period -> Int
periodDiff (Period endYear endMonth) (Period startYear startMonth) =
  12 * (endYear - startYear) + (endMonth - startMonth)

-- | Returns the next year in time after the given one.
nextYear :: Period -> Period
nextYear = forwardMonths monthsPerYear

periodToDay :: Period -> Day
periodToDay (Period yr mo) =
  fromGregorian (toInteger yr) mo $ gregorianMonthLength (toInteger yr) mo
