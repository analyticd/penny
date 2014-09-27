module Penny.Tree.TopLine.Error where

import qualified Penny.Tree.Time.Error as Time.Error
import qualified Penny.Tree.Date.Error as Date.Error

-- | Errors when converting a tree TopLine to a core TopLine.
data T
  = BadTime Time.Error.T
  | BadDate Date.Error.T
  | DuplicateDate
  | DuplicateTime
  | DuplicateFlag
  | DuplicateNumber
  | NoDate
  deriving (Eq, Ord, Show)
