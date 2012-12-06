{-# LANGUAGE ScopedTypeVariables, DeriveFunctor, TypeFamilies, RankNTypes #-}
module Model where

import Control.Applicative
import Control.Category
import Control.Lens
import Data.Ratio
import Interval
import Prelude hiding (id, (.))
import Prob (Prob(..))
--import Test.QuickCheck

data Sym a = Sym a | EOF
  deriving (Show, Eq, Functor)

sym :: Iso (Maybe a) (Maybe b) (Sym a) (Sym b)
sym = iso f g
  where
    f (Just a) = Sym a
    f Nothing = EOF

    g (Sym a) = Just a
    g EOF = Nothing

type PureModel a = Simple Prism PInterval a
type Model a = PureModel (Sym a)

prop_ModelRoundtrip :: Eq a => PureModel a -> a -> Bool
prop_ModelRoundtrip m x = review m x ^? m == Just x

stdEnumModel :: forall a. (Bounded a, Enum a) => PureModel a
stdEnumModel = prism enco deco
  where
    mini, maxi :: a
    mini = minBound
    maxi = maxBound

    num = toInteger (fromEnum maxi) - toInteger (fromEnum mini) + 1
    p1 = Prob $ 1 % num

    enco x = sizedInterval (Prob $ toInteger (fromEnum x) % num) p1

    deco rng@(PI (Prob a) _)
      = if rng `isSubintervalOf` enco (x :: a)
        then Right x
        else Left rng
      where x = toEnum (min i (fromEnum maxi))
            i = floor (a * fromInteger num)

maybeModel :: forall a. Prob -> PureModel a -> PureModel (Maybe a)
maybeModel p m = prism enco deco
  where
    r1 = PI 0 p
    r2 = PI p 1
    
    enco Nothing = r1
    enco (Just x) = embed r2 (review m x)

    deco :: PInterval -> Either PInterval (Maybe a)
    deco rng | rng `isSubintervalOf` r1 = Right Nothing
             | rng `isSubintervalOf` r2 = Right (unembed r2 rng ^? m)
             | otherwise = Left rng

--eofModel :: forall a. Prob -> PureModel a -> PureModel (Sym a)
--eofModel p m = maybeModel p m . sym