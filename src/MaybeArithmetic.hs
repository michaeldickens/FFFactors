{- |
Module      : MaybeArithmetic
Description : Defines arithmetic functions on the Maybe type.

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-09-01

-}

module MaybeArithmetic () where

instance (Num a) => Num (Maybe a) where
  (+) = (<*>) . (fmap (+))
  (*) = (<*>) . (fmap (*))
  (-) = (<*>) . (fmap (-))
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger x = Just (fromInteger x)

instance (Fractional a) => Fractional (Maybe a) where
  (/) = (<*>) . (fmap (/))
  recip = fmap recip
  fromRational x = Just (fromRational x)

instance (Floating a) => Floating (Maybe a) where
  pi = Just pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

-- maybeDiv :: Maybe Double -> Maybe Double -> Maybe Double
-- maybeDiv (Just x) (Just y) = Just (x / y)
-- maybeDiv _ _ = Nothing

-- Note: We could also implement MaybeArithmetic using monads, like
-- this.
-- (+) x y = x >>= \x' -> y >>= \y' -> Just (x' + y')
