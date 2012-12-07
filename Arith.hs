{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Arith where

import Control.Category
import Control.Lens
import Data.List
import Interval
import Model
import Prelude hiding (id, (.))
import Prob (Prob)

encodeStep :: Model a -> PInterval -> Sym a -> PInterval
encodeStep m outer x = x ^. remit (embedding outer . m)

encode :: Model a -> [a] -> Prob
encode m xs = midpoint $ foldl' (encodeStep m) initial (map Sym xs ++ [EOF])

decode :: Model a -> Prob -> [a]
decode m p = go (PI p p)
  where go inner = case inner ^? m of
                     Just w@(Sym c) -> c : go (inner ^?! embedding (w ^. remit m))
                     Just EOF -> []
                     Nothing -> error "Invalid stream"
