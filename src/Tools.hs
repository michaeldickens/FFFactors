{- |
Module      : Tools
Description : General functions used across various modules.

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-09-07

-}

module Tools
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
      -- * General utilities
    , mapWithIndex
    , proportionToPercent
    , asReturn
    , leveraged
    , lessFees
    , showAsPercent
    , shuffle
    , mapToOrderedList    
    , traceShowSelf
      -- * File utilities
    , printFileContents
    , forEachLine
    , forEachLineWithNum
      -- * AlmostEq
    , AlmostEq(..)
    , almostEq
    ) where

import MaybeArithmetic

import Control.Monad
import Data.Array.IO
import Data.Function (on)
import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Time.Calendar hiding (periodToDay)
import Debug.Trace
import System.IO
import System.Random
import Text.Printf


{- |

General-purpose functions.

-}

printFileContents :: FilePath -> IO ()
printFileContents filename = withFile filename ReadMode
       (\handle -> forEachLine handle putStrLn)

-- | Map over the given list, passing both the current element and the
-- current index to the mapping function.
mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f xs = map (uncurry f) (zip xs [0..(length xs - 1)])

-- | Given a function, call the function on each line of Handle until
-- handle signals EOF.
forEachLine :: Handle -> (String -> IO a) -> IO ()
forEachLine handle fun = forEachLineWithNum handle (\_ s -> fun s)

-- | Given a function, call the function on each line of Handle,
-- passing in the line number and the line itself.
--
-- Note: I think this is tail recursive but I'm not sure because
-- monads are hard.
forEachLineWithNum :: Handle -> (Int -> String -> IO a) -> IO ()
forEachLineWithNum handle fun = go 1 False
  where go :: Int -> Bool -> IO ()
        go _ True = return ()
        go num False = hGetLine handle >>= fun num >>=
                   (\_ -> hIsEOF handle) >>= go (num + 1)

-- | Converts a proportion to a percent, rounding to the nearest
-- integer.
proportionToPercent :: Double -> Int
proportionToPercent x = round $ 100 * x

-- | Converts a return (0.X) to a form where it can be used more
-- easily (1.X) and then converts back.
asReturn :: (Double -> Double) -> Double -> Double
asReturn f x = f (x + 1) - 1

-- | Given a leverage ratio and a margin interest rate (as a
-- proportion), converts a list of returns into leveraged returns.
leveraged :: Double -> Double -> [Double] -> [Double]
leveraged ratio interest =
  map (\x -> ((ratio * x) - interest * (ratio - 1)))

-- | Given a fee as proportion of asset value, applies fee to every
-- period.
lessFees :: Double -> [Double] -> [Double]
lessFees fee = map (asReturn $ \x -> (x * (1 - fee)))

showAsPercent :: Double -> String
showAsPercent x = printf "%.1f" $ 100 * x

-- | Randomly shuffle a list in O(n) time.
-- Copied from https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- makeArray xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    makeArray :: [a] -> IO (IOArray Int a)
    makeArray xs' = newListArray (1,n) xs'

-- | Return a list that's ordered on the map's key.
mapToOrderedList :: (Ord k) => Map.HashMap k a -> [a]
mapToOrderedList map' = map snd $ sortBy (compare `on` fst) $ Map.toList map'

traceShowSelf :: (Show a) => a -> a
traceShowSelf a = traceShow a a

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


{- |

AlmostEq

-}

class AlmostEq a where
  -- | Compare floats by checking that their absolute difference is below some
  -- threshold. Inappropriate for comparing very small numbers.
  --
  -- Two data structures are considered almost equal iff all their elements are
  -- almost equal.
  --
  -- Certain types, such as Int and Char, are considered almost equal iff they
  -- are equal.
  almostEq' :: Double -> a -> a -> Bool

instance AlmostEq Int where
  almostEq' _ = (==)

instance AlmostEq Integer where
  almostEq' _ = (==)

instance AlmostEq Period where
  almostEq' _ = (==)

instance AlmostEq Char where
  almostEq' _ = (==)

instance AlmostEq Double where
  almostEq' tolerance x y = abs (x - y) <= tolerance

instance (AlmostEq a, AlmostEq b) => AlmostEq (a, b) where
  almostEq' tolerance (x1, x2) (y1, y2) =
    (almostEq' tolerance x1 y1) && (almostEq' tolerance x2 y2)

instance AlmostEq a => AlmostEq [a] where
  almostEq' tolerance xs ys = all (uncurry $ almostEq' tolerance) $ zip xs ys

instance (Eq k, Hashable k, AlmostEq a) => AlmostEq (Map.HashMap k a) where
  -- | Two maps are considered almost equal if the keys are exactly equal and if
  -- the values corresponding to each key are almost equal.
  almostEq' tolerance map1 map2 =
    (Map.keys map1 == Map.keys map2)
    &&
    (all (\k -> almostEq' tolerance (map1 Map.! k) (map2 Map.! k)) $ Map.keys map1)

almostEq :: (AlmostEq a) => a -> a -> Bool
almostEq = almostEq' 1e-6
