{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Arith where

import Control.Lens
import Data.List
import Interval
import Model
import Prob (Prob)

encodeStep :: Model a -> PInterval -> Sym a -> PInterval
encodeStep m outer x = embed outer (review m x)

encode :: Model a -> [a] -> Prob
encode m xs = midpoint $ foldl' (encodeStep m) initial (map Sym xs ++ [EOF])

decode :: Model a -> Prob -> [a]
decode m p = go (PI p p)
  where go inner = case inner ^? m of
                     Just w@(Sym c) -> c : go (unembed (review m w) inner)
                     Just EOF -> []
                     Nothing -> error "Invalid stream"
