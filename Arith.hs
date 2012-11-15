{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Arith where

--import Data.Ratio
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Data.List
import System.Random
import Test.QuickCheck

newtype Prob = Prob { unProb :: Rational }
  deriving (Eq, Ord, Num, Fractional)

probIsValid :: Prob -> Bool
probIsValid (Prob x) = x >= 0 && x <= 1

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

data PInterval = PI { start :: Prob, end :: Prob }
  deriving (Show, Eq)

piIsValid :: PInterval -> Bool
piIsValid (PI a b) = probIsValid a && probIsValid b && a < b

instance Arbitrary PInterval where
  arbitrary = do [a, b] <- sort <$> replicateM 2 arbitrary
                 return (PI a b)

data Model a = Model { enc :: a -> PInterval, dec :: Prob -> a }

initial :: PInterval
initial = PI 0 1

midpoint :: PInterval -> Prob
midpoint (PI a b) = (a + b) / 2

embed :: PInterval -> PInterval -> PInterval
embed (PI a b) (PI x y) = PI (a + x*l) (b-(1-y)*l)
  where l = b - a

prop_embedConstrains :: PInterval -> PInterval -> Bool
prop_embedConstrains outer@(PI oa ob) inner = let (PI a b) = embed outer inner
                                              in a >= oa && b <= ob

prop_embedValid :: PInterval -> PInterval -> Bool
prop_embedValid outer inner = piIsValid (embed outer inner)

unembed :: PInterval -> PInterval -> PInterval
unembed (PI a b) (PI x y) = PI ((x - a) / l) (1 - (b - y) / l)
  where l = b - a

prop_unembedEmbed :: PInterval -> PInterval -> Bool
prop_unembedEmbed outer inner = unembed outer (embed outer inner) == inner

encodeStep :: Model a -> a -> PInterval -> PInterval
encodeStep m x outer = embed outer (enc m x)

encode :: Model a -> [a] -> Prob
encode model xs = midpoint $ foldl' (flip $ encodeStep model) initial xs

decode :: Model a -> Prob -> [(a, PInterval)]
decode model p = go (PI p p)
  where go inner = (c, inner) : go (unembed outer inner)
          where c = dec model (midpoint inner)
                outer = enc model c

stdBoolModel :: Model Bool
stdBoolModel = Model { enc = enco, dec = deco }
  where enco False = PI 0 0.5
        enco True = PI 0.5 1

        deco x | x <= 0.5  = False
               | otherwise = True