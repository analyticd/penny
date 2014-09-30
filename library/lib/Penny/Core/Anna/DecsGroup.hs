module Penny.Core.Anna.DecsGroup where

import qualified Penny.Core.DecDecs as DecDecs
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Core.Decems as Decems
import qualified Penny.Natural.NonZero as NonZero

data T r = T
  { grouper :: r
  , decDecs :: DecDecs.T
  } deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser p = T <$> p <*> DecDecs.parser

toDecems :: T r -> Decems.T
toDecems (T _ a) = DecDecs.toDecems a

numDigits :: T a -> NonZero.T
numDigits (T _ ds) = DecDecs.numDigits ds
