module Penny.Core.Anna.SeqDecs where

import qualified Penny.Core.Anna.DecsGroup as DecsGroup
import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as S
import Text.Parsec.Text
import qualified Penny.Tree.Parsec as P
import Data.Monoid
import qualified Data.Foldable as F
import qualified Penny.Core.Decems as Decems
import qualified Penny.Natural.Unsigned as Unsigned
import qualified Penny.Natural.NonZero as NonZero

newtype T r = T { toSeq :: Seq (DecsGroup.T r) }
  deriving (Eq, Ord, Show)

instance Monoid (T a) where
  mempty = T S.empty
  mappend (T a) (T b) = T (a <> b)

parser :: Parser a -> Parser (T a)
parser p = fmap T $ P.seq (DecsGroup.parser p)

empty :: T a
empty = T S.empty

toDecems :: T a -> Decems.T
toDecems = F.foldl (<>) Decems.empty
  . fmap DecsGroup.toDecems . toSeq

numDigits :: T a -> Unsigned.T
numDigits (T sqn) = case S.viewl sqn of
  EmptyL -> Unsigned.zero
  x :< xs -> NonZero.toUnsigned
    $ F.foldl (\dg nz -> DecsGroup.numDigits `NonZero.add` nz)
      (DecsGroup.numDigits x) xs
