{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Arith where

import Control.Lens
import Data.List
import Interval
import Model
import Prob (Prob)

encodeStep :: Model a -> Sym a -> PInterval -> PInterval
encodeStep m x outer = embed outer (review m x)

encode :: Model a -> [a] -> Prob
encode m xs = midpoint $ foldl' (flip $ encodeStep m) initial (map Sym xs ++ [EOF])

decode :: Model a -> Prob -> [a]
decode m p = go (PI p p)
  where go inner = case inner ^? m of
                     Just w@(Sym c) -> c : go (unembed (review m w) inner)
                     Just EOF -> []
                     Nothing -> error "Invalid stream"
