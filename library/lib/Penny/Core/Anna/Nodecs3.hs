module Penny.Core.Anna.Nodecs3 where

import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Natural.NonZero as NonZero

data T r = T
  { novDecs :: NovDecs.T
  , seqDecs :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

numDigits :: T a -> NonZero.T
numDigits (T nd sd) =
  NovDecs.numDigits nd `NonZero.addUnsigned` (SeqDecs.numDigits sd)

toNovDecs :: T a -> NovDecs.T
toNovDecs (T nd sd) =
  nd `NovDecs.appendDecems` (SeqDecs.toDecems sd)
