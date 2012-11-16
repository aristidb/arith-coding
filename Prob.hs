{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Prob where

import Data.Word
import Data.Ratio
import Control.Arrow (second)

newtype Prob = Prob Rational
  deriving (Eq, Ord, Num, Fractional)

probIsValid :: Prob -> Bool
probIsValid (Prob x) = x >= 0 && x <= 1

instance Show Prob where
  showsPrec d (Prob x) = showsPrec d (fromRational x :: Double)

instance Bounded Prob where
  minBound = 0
  maxBound = 1

bits :: Prob -> Int
bits 0 = error "Infinite bits"
bits x | x <= 0.5 = 1 + bits (x*2)
bits _ = 0

twoTo64 :: Integer
twoTo64 = 2 ^ (64 :: Int)

fromWord64 :: Word64 -> Prob
fromWord64 x = Prob $ toInteger x % twoTo64

readWord64 :: Prob -> (Word64, Prob)
readWord64 (Prob x) = second Prob $ properFraction (x*fromInteger twoTo64)
