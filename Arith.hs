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

data PInterval = PI Prob Prob
  deriving (Show, Eq)

instance Arbitrary PInterval where
  arbitrary = do [a, b] <- sort <$> replicateM 2 arbitrary
                 return (PI a b)

data Iso a b = Iso { to :: a -> b
                   , from :: b -> a }

type Model a = Iso PInterval a

initial :: PInterval
initial = PI 0 1

midpoint :: PInterval -> Prob
midpoint (PI a b) = (a + b) / 2

embed :: PInterval -> PInterval -> PInterval
embed (PI a b) (PI x y) = PI (a + x/l) (b-(1-y)/l)
  where l = b - a

unembed :: PInterval -> PInterval -> PInterval
unembed (PI a b) (PI x y) = PI ((x - a) * l) (1 - l * (b - y))
  where l = b - a

prop_unembedEmbed :: PInterval -> PInterval -> Bool
prop_unembedEmbed outer inner = unembed outer (embed outer inner) == inner

encodeStep :: Model a -> PInterval -> a -> PInterval
encodeStep (Iso f _) (PI a b) x = undefined

encode :: Model a -> [a] -> Prob
encode model xs = midpoint $ foldl' (encodeStep model) initial xs
