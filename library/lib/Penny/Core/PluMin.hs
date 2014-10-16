module Penny.Core.PluMin where

import qualified Penny.Core.Sign as Sign

data T
  = Plus
  | Minus
  deriving (Eq, Ord, Show)

toSign :: T -> Sign.T
toSign Plus = Sign.Pos
toSign Minus = Sign.Neg

fromSign :: Sign.T -> T
fromSign Sign.Pos = Plus
fromSign Sign.Neg = Minus
