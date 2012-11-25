{-# LANGUAGE ScopedTypeVariables, DeriveFunctor #-}
module Model where

import Interval
import Prob (Prob(..))
import Data.Ratio
import Control.Applicative
--import Test.QuickCheck

data Decode a = Sym a | EOF | NoSym
  deriving (Show, Eq, Functor)

data Model a = Model {
    enc :: a -> PInterval
  , eofPI :: PInterval 
  , dec :: PInterval -> Decode a
  }

prop_ModelRoundtrip :: Eq a => Model a -> a -> Bool
prop_ModelRoundtrip m a = dec m (enc m a) == Sym a

stdEnumModel :: forall a. (Bounded a, Enum a) => Model a
stdEnumModel = Model { enc = enco, eofPI = PI 0 0, dec = deco }
  where
    mini, maxi :: a
    mini = minBound
    maxi = maxBound

    num = toInteger (fromEnum maxi) - toInteger (fromEnum mini) + 1
    p1 = Prob $ 1 % num

    enco x = sizedInterval (Prob $ toInteger (fromEnum x) % num) p1

    deco rng@(PI (Prob a) _)
      = if rng `isSubintervalOf` enco x
        then Sym x
        else NoSym
      where x = toEnum (min i (fromEnum maxi))
            i = floor (a * fromInteger num)

maybeModel :: forall a. Prob -> Model a -> Model (Maybe a)
maybeModel p m = Model { enc = enco, eofPI = PI 0 0, dec = deco }
  where
    r1 = PI 0 p
    r2 = PI p 1
    
    enco Nothing = r1
    enco (Just x) = embed r2 (enc m x)

    deco rng | rng `isSubintervalOf` r1 = Sym Nothing
             | rng `isSubintervalOf` r2 = Just <$> dec m (unembed r2 rng)
             | otherwise = NoSym

eofModel :: forall a. Prob -> Model a -> Model a
eofModel p m = Model { enc = enc mMod . Just
                     , eofPI = PI 0 p
                     , dec = adaptDec . dec mMod }
  where mMod = maybeModel p m

        adaptDec (Sym (Just x)) = Sym x
        adaptDec (Sym Nothing) = EOF
        adaptDec EOF = EOF
        adaptDec NoSym = NoSym
