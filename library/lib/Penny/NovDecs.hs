module Penny.NovDecs where

import Deka.Native.Abstract
import qualified Penny.Decems as Decems
import qualified Penny.Pos as Pos

data T = T
  { novem :: Novem
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

toPos :: T -> Pos.T
toPos (T n d) = Pos.addNonNeg (Pos.fromNovem n) (Decems.toNonNeg d)
