{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Prob where

import Data.Word
import Data.Ratio
import Control.Arrow (first, second)
import System.Random
import Test.QuickCheck

newtype Prob = Prob Rational
  deriving (Eq, Ord, Num, Fractional)

isValid :: Prob -> Bool
isValid (Prob x) = x >= 0 && x <= 1

instance Show Prob where
  showsPrec d (Prob x) = showsPrec d (fromRational x :: Double)

instance Bounded Prob where
  minBound = 0
  maxBound = 1

instance Random Prob where
  random = randomR (minBound, maxBound)
  randomR (Prob mini, Prob maxi) = first (Prob . toRational)
                                   . randomR (fromRational mini :: Double, fromRational maxi)
instance Arbitrary Prob where
  arbitrary = arbitraryBoundedRandom

bits :: Prob -> Int
bits 0 = error "Infinite bits"
bits x | x <= 0.5 = 1 + bits (x*2)
bits _ = 0

twoTo64MinusOne :: Integer
twoTo64MinusOne = 2 ^ (64 :: Int) - 1

fromWord64 :: Word64 -> Prob
fromWord64 x = Prob $ toInteger x % twoTo64MinusOne

readWord64 :: Prob -> (Word64, Prob)
readWord64 (Prob x) = second Prob $ properFraction (x*fromInteger twoTo64MinusOne)
