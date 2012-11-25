{-# LANGUAGE ScopedTypeVariables, DeriveFunctor #-}
module Model where

import Interval
import Prob (Prob(..))
import Data.Ratio
import Control.Applicative
import Control.Lens
--import Test.QuickCheck

data Sym a = Sym a | EOF
  deriving (Show, Eq, Functor)

sym :: Iso (Maybe a) (Maybe b) (Sym a) (Sym b)
sym = isos f g f g
  where
    f (Just a) = Sym a
    f Nothing = EOF

    g (Sym a) = Just a
    g EOF = Nothing

data Decode a = Decode a | NoDecode
  deriving (Show, Eq, Functor)

data PureModel a = Model {
    enc :: a -> PInterval
  , dec :: PInterval -> Decode a
  }

type Model a = PureModel (Sym a)

prop_ModelRoundtrip :: Eq a => PureModel a -> a -> Bool
prop_ModelRoundtrip m a = dec m (enc m a) == Decode a

stdEnumModel :: forall a. (Bounded a, Enum a) => PureModel a
stdEnumModel = Model { enc = enco, dec = deco }
  where
    mini, maxi :: a
    mini = minBound
    maxi = maxBound

    num = toInteger (fromEnum maxi) - toInteger (fromEnum mini) + 1
    p1 = Prob $ 1 % num

    enco x = sizedInterval (Prob $ toInteger (fromEnum x) % num) p1

    deco rng@(PI (Prob a) _)
      = if rng `isSubintervalOf` enco (x :: a)
        then Decode x
        else NoDecode
      where x = toEnum (min i (fromEnum maxi))
            i = floor (a * fromInteger num)

maybeModel :: forall a. Prob -> PureModel a -> PureModel (Maybe a)
maybeModel p m = Model { enc = enco, dec = deco }
  where
    r1 = PI 0 p
    r2 = PI p 1
    
    enco Nothing = r1
    enco (Just x) = embed r2 (enc m x)

    deco rng | rng `isSubintervalOf` r1 = Decode Nothing
             | rng `isSubintervalOf` r2 = Just <$> dec m (unembed r2 rng)
             | otherwise = NoDecode

eofModel :: forall a. Prob -> PureModel a -> Model a
eofModel p m = Model { enc = enc mMod . review sym
                     , dec = view (mapping sym) . dec mMod }
  where mMod = maybeModel p m
