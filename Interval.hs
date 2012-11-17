module Interval where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Word
import qualified Prob
import           Prob (Prob)
import           Test.QuickCheck

data PInterval = PI { start :: Prob, end :: Prob }

isSubintervalOf :: PInterval -> PInterval -> Bool
PI a b `isSubintervalOf` PI x y = a >= x && b <= y

isValid :: PInterval -> Bool
isValid (PI a b) = Prob.isValid a && Prob.isValid b && a < b

instance Arbitrary PInterval where
  arbitrary = do [a, b] <- sort <$> replicateM 2 arbitrary
                 return (PI a b)

bits :: PInterval -> Int
bits r = Prob.bits (end r - start r)

initial :: PInterval
initial = PI 0 1

midpoint :: PInterval -> Prob
midpoint (PI a b) = (a + b) / 2

embed :: PInterval -> PInterval -> PInterval
embed (PI a b) (PI x y) = PI (a + x*l) (b-(1-y)*l)
  where l = b - a

fromWord64 :: Word64 -> PInterval
fromWord64 x = PI p (p + 1/fromInteger Prob.twoTo64)
  where p = Prob.fromWord64 x

appendWord64 :: PInterval -> Word64 -> PInterval
appendWord64 outer = embed outer . fromWord64

readWord64 :: PInterval -> (Word64, PInterval)
readWord64 rng = undefined