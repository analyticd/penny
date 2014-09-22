module Penny.Tree.LZ6 where

import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.Zeroes as Zeroes
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Tree.Parsec as P
import Text.Parsec (choice)
import qualified Penny.Core.Anna.SeqDecs as SeqDecs

data T a
  = Novem NovDecs.T (SeqDecs.T a)
  | Zero Zeroes.T
         (Maybe (Either (NovDecs.T, SeqDecs.T a)
                        (a, T a)))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa = choice
  [ Novem <$> NovDecs.parser <*> SeqDecs.parser pa
  , Zero <$> Zeroes.parser
    <*> optional (P.either ((,) <$> NovDecs.parser <*> SeqDecs.parser pa)
                           ((,) <$> pa <*> parser pa))
  ]
