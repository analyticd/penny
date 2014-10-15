module Penny.Core.DecDecs where

import qualified Penny.Natural.Decem as Decem
import qualified Penny.Core.Decems as Decems
import Control.Monad
import Text.Parsec.Text
import Data.Sequence ((<|))
import Data.Monoid
import qualified Penny.Natural.NonZero as NonZero

data T = T
  { decem :: Decem.T
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

append :: T -> T -> T
append (T a1 as) (T b1 bs) = T a1
  (as <> Decems.cons b1 bs)

parser :: Parser T
parser = liftM2 T Decem.parser Decems.parser

toDecems :: T -> Decems.T
toDecems (T d (Decems.T ds)) = Decems.T (d <| ds)

numDigits :: T -> NonZero.T
numDigits (T _ ds) =
  NonZero.one `NonZero.addUnsigned` (Decems.numDigits ds)
