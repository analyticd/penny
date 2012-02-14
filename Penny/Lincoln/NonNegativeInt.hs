module Penny.Lincoln.NonNegativeInt where

import Data.Maybe (fromMaybe)
import Data.Word (Word)

import qualified Penny.Lincoln.Classes as C

newtype T = T { unT :: Word }
            deriving (Eq, Ord, Show)

instance C.NonNeg T where
  add (T l) (T r) = T (l + r)
  subt (T l) (T r) =
    if r > l then Nothing else Just (T (l - r))
  mult (T l) (T r) = T (l * r)
  zero = T 0
  fromInt f = let fi = fromIntegral f in
    if fi < 0 then Nothing else Just (T fi)
  unsafeFromInt f =
    fromMaybe (error "NonNegativeInteger is negative") (C.fromInt f)

instance C.NonNegInt T where
  toInt (T w) = fromIntegral w
