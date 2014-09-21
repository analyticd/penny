module Penny.Core.Anna.DecDecs where

import qualified Penny.Core.Anna.Decems as Decems
import Deka.Native
import qualified Penny.Tree.Parsec as P
import Control.Monad
import Text.Parsec.Text
import Data.Sequence ((<|))

data T = T
  { decem :: Decem
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

parser :: Parser T
parser = liftM2 T P.decem Decems.parser

toDecems :: T -> Decems.T
toDecems (T d (Decems.T ds)) = Decems.T (d <| ds)
