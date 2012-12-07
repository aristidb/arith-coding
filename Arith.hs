{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Arith where

import Control.Category
import Control.Lens
import Data.List
import Interval
import Model
import Prelude hiding (id, (.))
import Prob (Prob)

encodeStep :: Model a -> Sym a -> PInterval -> PInterval
encodeStep m x inner = (inner, x) ^. remit m

encode :: Model a -> [a] -> PInterval
encode m xs = foldr (encodeStep m) initial (map Sym xs ++ [EOF])

decode :: Model a -> PInterval -> [a]
decode m = go
  where go inner = case inner ^? m of
                     Just (r, w@(Sym c)) -> c : go r
                     Just (_, EOF) -> []
                     Nothing -> error "Invalid stream"

prop_roundtripA :: Eq a => Model a -> [a] -> Bool
prop_roundtripA m xs = decode m (encode m xs) == xs

--prop_roundtripB :: Eq a => Model a -> PInterval -> Bool
