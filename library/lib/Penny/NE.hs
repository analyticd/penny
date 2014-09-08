module Penny.NE where

import qualified Data.Sequence as Seq
import Data.Sequence ((<|))
import qualified Penny.Pos as Pos
import qualified Penny.NonNeg as NonNeg

data T a b = T
  { first :: a
  , rest :: Seq.Seq b
  } deriving (Eq, Ord, Show)

toSeq :: T a a -> Seq.Seq a
toSeq (T f r) = f <| r

length :: T a b -> Pos.T
length (T _ b) = Pos.one `Pos.addNonNeg` (NonNeg.length b)
