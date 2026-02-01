{- |
Module      : Read
Description : Functions for reading text into internal data formats

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2014-12-22

-}

module Read
    ( readDouble
    , readDoubleUnsafe
    , readInt
    , readIntUnsafe
    , readIntegerUnsafe
    ) where

import qualified Data.Text as Text
import Data.Text.Read

readWrapper :: Reader a -> Text.Text -> Maybe a
readWrapper reader text = case (reader text) of
  Left _ -> Nothing
  Right (value, _) -> Just value

readInt :: Text.Text -> Maybe Int
readInt = readWrapper decimal

readUnsafeWrapper :: Reader a -> Text.Text -> a
readUnsafeWrapper reader text = case (reader text) of
  Left message -> error $ "\"" ++ Text.unpack text ++ "\": " ++ message
  Right (value, _) -> value

-- | Reads an Int where the given string must contain a valid Int. If
-- it does not, this raises an error.
readIntUnsafe :: Text.Text -> Int
readIntUnsafe = readUnsafeWrapper decimal

readIntegerUnsafe :: Text.Text -> Integer
readIntegerUnsafe = readUnsafeWrapper decimal

readDouble :: Text.Text -> Maybe Double
readDouble = readWrapper double

readDoubleUnsafe :: Text.Text -> Double
readDoubleUnsafe = readUnsafeWrapper double
