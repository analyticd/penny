module Penny.NE where

import qualified Data.Sequence as Seq
import Data.Sequence ((<|))
import qualified Penny.NonZero as NonZero
import qualified Penny.Unsigned as Unsigned

data T a b = T
  { first :: a
  , rest :: Seq.Seq b
  } deriving (Eq, Ord, Show)

toSeq :: T a a -> Seq.Seq a
toSeq (T f r) = f <| r

length :: T a b -> NonZero.T
length (T _ b) = NonZero.one `NonZero.addUnsigned` (Unsigned.length b)
