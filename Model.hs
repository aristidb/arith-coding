{-# LANGUAGE ScopedTypeVariables, DeriveFunctor, TypeFamilies, RankNTypes, ViewPatterns, TupleSections #-}
module Model where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens
import Data.Monoid
import Data.Ratio
import Interval
import Prelude hiding (id, (.))
import Prob (Prob(..))
import Test.QuickCheck
import Test.QuickCheck.Property

data Sym a = Sym a | EOF
  deriving (Show, Eq, Functor)

sym :: Iso (Maybe a) (Maybe b) (Sym a) (Sym b)
sym = iso f g
  where
    f (Just a) = Sym a
    f Nothing = EOF

    g (Sym a) = Just a
    g EOF = Nothing

type PureModel a = Simple Prism PInterval (PInterval, a)
type Model a = PureModel (Sym a)

prop_ModelRoundtripA :: Eq a => PureModel a -> (PInterval, a) -> Bool
prop_ModelRoundtripA m x = review m x ^? m == Just x

prop_ModelRoundtripB :: Eq a => PureModel a -> PInterval -> Property
prop_ModelRoundtripB m x = case x ^? m of Nothing -> property rejected
                                          Just y -> property $ y ^. remit m == x

prop_ModelRoundtrip :: (Show a, Arbitrary a, Eq a) => PureModel a -> Property
prop_ModelRoundtrip m = prop_ModelRoundtripA m .&. prop_ModelRoundtripB m

mkModel :: (a -> PInterval) -> (PInterval -> Maybe (PInterval, a)) -> PureModel a
mkModel enco deco = prism
                      (\(r,x) -> review (embedding (enco x)) r)
                      (\x -> maybe (Left x) Right (deco x))

stdEnumModel :: forall a. (Bounded a, Enum a) => PureModel a
stdEnumModel = mkModel enco deco
  where
    mini, maxi :: Int
    mini = fromEnum (minBound :: a)
    maxi = fromEnum (maxBound :: a)

    num = toInteger maxi - toInteger mini + 1
    p1 = Prob $ 1 % num

    enco x = sizedInterval start p1
      where start = Prob $ toInteger (fromEnum x) % num

    deco rng@(PI (Prob a) _)
      = rng ^? embedding outer <&> (,x)
      where x = toEnum (min i maxi)
            outer = enco x
            i = floor (a * fromInteger num)

maybeModel :: forall a. Prob -> PureModel a -> PureModel (Maybe a)
maybeModel p m = mkModel enco deco
  where
    r1 = PI 0 p
    r2 = PI p 1
    
    enco Nothing = r1
    enco (Just x) = review (embedding r2) (review m (mempty, x))

    deco (preview (embedding r1) -> Just i1) = Just (i1, Nothing)
    deco (preview (embedding r2) -> Just i2) = i2 ^? m <&> fmap Just
    deco _                                   = Nothing

eofModel :: Prob -> PureModel a -> Model a
eofModel p m = maybeModel p m . mapping sym
