module Penny.Lincoln.View where

import Data.Sequence
import qualified Penny.Lincoln.Ent as Ent

data T a = T
  { left :: Seq (Ent.T a)
  , current :: Ent.T a
  , right :: Seq (Ent.T a)
  } deriving (Eq, Ord, Show)
