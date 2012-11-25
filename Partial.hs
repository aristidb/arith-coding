module Partial where

import           Data.Word
import           Interval
import qualified Interval as I

data Partial m = Partial {
    current :: PInterval
  , next :: m (Partial m)
  }

fromWord64M :: Monad m => m Word64 -> Partial m
fromWord64M retr = let initial = PI 0 1 in  Partial { current = initial, next = go initial }
  where go rng = do v <- retr
                    let w = appendWord64 rng v
                    return Partial { current = w, next = go w }

toWord64M :: Monad m => Partial m -> m Word64
toWord64M = undefined
