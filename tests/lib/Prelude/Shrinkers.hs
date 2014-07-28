module Prelude.Shrinkers where

import Prelude hiding (maybe)

maybe :: (a -> [a]) -> Maybe a -> [Maybe a]
maybe s m = case m of
  Nothing -> []
  Just x -> Nothing : map Just (s x)
