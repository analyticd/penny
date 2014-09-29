module Penny.Tree.LZ6 where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Core.Anna.Zeroes as Zeroes
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Tree.Parsec as P
import Text.Parsec (choice)
import qualified Penny.Core.Anna.SeqDecsNE as SeqDecsNE

data T a
  = Novem NovDecs.T (Maybe (SeqDecsNE.T a))
  | Zero Zeroes.T
         (Maybe (Either (NovDecs.T, Maybe (SeqDecsNE.T a))
                        (a, T a)))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa = choice
  [ Novem <$> NovDecs.parser <*> optional (SeqDecsNE.parser pa)
  , Zero <$> Zeroes.parser
    <*> optional
        (P.either ((,) <$> NovDecs.parser
                       <*> optional (SeqDecsNE.parser pa))
                           ((,) <$> pa <*> parser pa))
  ]
