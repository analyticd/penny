module Penny.Tree.Posting.Error where

import qualified Penny.Tree.Ingot.Error as Ingot.Error

data T
  = DuplicateFlag
  | DuplicateNumber
  | DuplicatePayee
  | DuplicateAccount
  | DuplicateSide
  | DuplicateCommodity
  | DuplicateIngot
  | Ingot Ingot.Error.T
  | NoAccount
  deriving (Eq, Ord, Show)
