{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Arith where

import           Data.List
import           Interval
import           Model
import           Prob (Prob)

encodeStep :: Model a -> a -> PInterval -> PInterval
encodeStep m x outer = embed outer (enc m x)

encode :: Model a -> [a] -> Prob
encode model xs = midpoint $ embed (foldl' (flip $ encodeStep model) initial xs) (eofPI model)

decode :: Model a -> Prob -> [a]
decode model p = go (PI p p)
  where go inner = case dec model inner of
                     Sym c -> c : go (unembed (enc model c) inner)
                     EOF -> []
                     NoSym -> error "Invalid stream"
