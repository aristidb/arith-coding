{-# LANGUAGE ScopedTypeVariables #-}
module Model where

import Interval
import Prob (Prob(..))
import Data.Ratio
--import Test.QuickCheck

data Model a = Model {
    enc :: a -> PInterval
  , dec :: PInterval -> Maybe a
  }

prop_ModelRoundtrip :: Eq a => Model a -> a -> Bool
prop_ModelRoundtrip m a = dec m (enc m a) == Just a

stdEnumModel :: forall a. (Bounded a, Enum a) => Model a
stdEnumModel = Model { enc = enco, dec = deco }
  where
    num = toInteger (fromEnum (maxBound :: a)) - toInteger (fromEnum (minBound :: a)) + 1
    p1 = Prob $ 1 % num

    enco x = sizedInterval (Prob $ toInteger (fromEnum x) % num) p1

    deco rng@(PI (Prob a) _)
      = if rng `isSubintervalOf` enco x
        then Just x
        else Nothing
      where x = toEnum (floor (a * fromInteger num))
