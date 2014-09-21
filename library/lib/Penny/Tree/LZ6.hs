module Penny.Tree.LZ6 where

import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.DecDecs as DecDecs
import qualified Penny.Core.Anna.Zeroes as Zeroes
import Data.Sequence (Seq)
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Tree.Parsec as P
import Text.Parsec (choice)

data T a
  = Novem NovDecs.T (Seq (a, DecDecs.T))
  | Zero Zeroes.T
         (Maybe (Either (NovDecs.T, Seq (a, DecDecs.T))
                        (a, T a)))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa = choice
  [ Novem <$> NovDecs.parser <*> sq
  , Zero <$> Zeroes.parser
    <*> optional (P.either ((,) <$> NovDecs.parser <*> sq)
                           ((,) <$> pa <*> parser pa))
  ]
  where
    sq = P.seq ((,) <$> pa <*> DecDecs.parser)
