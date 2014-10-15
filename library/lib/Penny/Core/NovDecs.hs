module Penny.Core.NovDecs where

import qualified Penny.Natural.Decem as Decem
import qualified Penny.Natural.Novem as Novem
import qualified Penny.Core.Decems as Decems
import qualified Penny.Core.DecDecs as DecDecs
import qualified Penny.Natural.NonZero as NonZero
import Text.Parsec.Text
import Control.Applicative
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Sequence ((<|))

data T = T
  { novem :: Novem.T
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

toNonZero :: T -> NonZero.T
toNonZero (T n d) =
  NonZero.addUnsigned (NonZero.fromNovem n) (Decems.toUnsigned d)

fromNonZero :: NonZero.T -> T
fromNonZero = go Seq.empty . NonZero.toInteger
  where
    go soFar i
      | q == 0 = case Novem.fromInt r of
          Just nv -> T nv (Decems.T soFar)
          Nothing -> error "fromNonZero: error 1"
      | otherwise = case Decem.fromInt r of
          Just dc -> go (dc <| soFar) q
          Nothing -> error "fromNonZero: error 2"
      where
        (q, r) = i `divMod` 10

parser :: Parser T
parser = T <$> Novem.parser <*> Decems.parser

toDecDecs :: T -> DecDecs.T
toDecDecs (T d1 ds) = DecDecs.T (Decem.Novem d1) ds

appendDecems :: T -> Decems.T -> T
appendDecems (T n ds1) ds2 = T n (ds1 <> ds2)

numDigits :: T -> NonZero.T
numDigits (T _ ds) = NonZero.one `NonZero.addUnsigned`
  (Decems.numDigits ds)
