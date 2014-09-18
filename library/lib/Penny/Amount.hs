module Penny.Amount where

import qualified Penny.Amount.Pre as Pre
import qualified Penny.Amount.Post as Post

-- BROKEN needs to deal differently with different radii
data T a
  = Pre (Pre.T a)
  | Post (Post.T a)
  deriving (Eq, Ord, Show)
