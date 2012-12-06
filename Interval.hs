module Interval where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Monoid
import           Data.Word
import qualified Prob
import           Prob (Prob)
import           Test.QuickCheck

data PInterval = PI { start :: Prob, end :: Prob }
  deriving (Eq, Show)

sizedInterval :: Prob -> Prob -> PInterval
sizedInterval a l = PI a (a + l)

isSubintervalOf :: PInterval -> PInterval -> Bool
PI a b `isSubintervalOf` PI x y = a >= x && b <= y

isValid :: PInterval -> Bool
isValid (PI a b) = Prob.isValid a && Prob.isValid b && a < b

instance Monoid PInterval where
  mempty = PI 0 1
  mappend = embed

prop_leftIdMonoid :: PInterval -> Bool
prop_leftIdMonoid x = mempty `mappend` x == x

prop_rightIdMonoid :: PInterval -> Bool
prop_rightIdMonoid x = x `mappend` mempty == x

prop_assocMonoid :: PInterval -> PInterval -> PInterval -> Bool
prop_assocMonoid a b c = a `mappend` (b `mappend` c) == (a `mappend` b) `mappend` c

instance Arbitrary PInterval where
  arbitrary = do [a, b] <- sort <$> replicateM 2 arbitrary
                 return (PI a b)

bits :: PInterval -> Int
bits r = Prob.bits (end r - start r)

midpoint :: PInterval -> Prob
midpoint (PI a b) = (a + b) / 2

initial :: PInterval
initial = PI 0 1

embed :: PInterval -> PInterval -> PInterval
embed (PI a b) (PI x y) = PI (a + x*l) (b-(1-y)*l)
  where l = b - a

unembed :: PInterval -> PInterval -> PInterval
unembed (PI a b) (PI x y) = PI ((x - a) / l) (1 - (b - y) / l)
  where l = b - a

prop_embedConstrains :: PInterval -> PInterval -> Bool
prop_embedConstrains outer inner = embed outer inner `isSubintervalOf` outer

prop_embedValid :: PInterval -> PInterval -> Bool
prop_embedValid outer inner = isValid (embed outer inner)

prop_unembedEmbed :: PInterval -> PInterval -> Bool
prop_unembedEmbed outer inner = unembed outer (embed outer inner) == inner

fromWord64 :: Word64 -> PInterval
fromWord64 x = PI p (p + 1/fromInteger Prob.twoTo64MinusOne)
  where p = Prob.fromWord64 x

appendWord64 :: PInterval -> Word64 -> PInterval
appendWord64 outer = embed outer . fromWord64

readWord64 :: PInterval -> Either Word64 (Word64, PInterval)
readWord64 (PI a b) = if wa == wb
                      then Right (wa, PI pa pb)
                      else Left ((wa + wb) `div` 2)
  where (wa, pa) = Prob.readWord64 a
        (wb, pb) = Prob.readWord64 b
