{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Arith where

import           Data.List
import           Interval
import           Model
import           Prob (Prob)

encodeStep :: Model a -> Sym a -> PInterval -> PInterval
encodeStep m x outer = embed outer (enc m x)

encode :: Model a -> [a] -> Prob
encode model xs = midpoint $ foldl' (flip $ encodeStep model) initial (map Sym xs ++ [EOF])

decode :: Model a -> Prob -> [a]
decode model p = go (PI p p)
  where go inner = case dec model inner of
                     Decode w@(Sym c) -> c : go (unembed (enc model w) inner)
                     Decode EOF -> []
                     NoDecode -> error "Invalid stream"
