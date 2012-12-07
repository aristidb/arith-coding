{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Arith where

import Control.Applicative
import Control.Category
import Control.Lens
import Data.List
import Interval
import Model
import Prelude hiding (id, (.))
import Prob (Prob)
import Test.QuickCheck
import Test.QuickCheck.Property

encodeStep :: Model a -> Sym a -> PInterval -> PInterval
encodeStep m x inner = (inner, x) ^. remit m

encode :: Model a -> [a] -> PInterval
encode m xs = foldr (encodeStep m) initial (map Sym xs ++ [EOF])

decode :: Model a -> PInterval -> Maybe [a]
decode m = go
  where go inner = case inner ^? m of
                     Just (r, w@(Sym c)) -> (c :) <$> go r
                     Just (_, EOF) -> Just []
                     Nothing -> Nothing

prop_roundtripA :: Eq a => Model a -> [a] -> Bool
prop_roundtripA m xs = decode m (encode m xs) == Just xs

prop_roundtripB :: Eq a => Model a -> PInterval -> Property
prop_roundtripB m r = case decode m r of
                        Nothing -> property rejected
                        Just xs -> property $ r `isSubintervalOf` encode m xs

prop_roundtrip :: (Eq a, Show a, Arbitrary a) => Model a -> Property
prop_roundtrip m = prop_roundtripA m .&. prop_roundtripB m
