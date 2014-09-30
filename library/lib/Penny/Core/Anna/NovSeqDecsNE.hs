module Penny.Core.Anna.NovSeqDecsNE where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Core.Anna.SeqDecsNE as SeqDecsNE
import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Natural.NonZero as NonZero

data T r = T
  { novDecs :: NovDecs.T
  , seqDecsNE :: SeqDecsNE.T r
  } deriving (Eq, Ord, Show)

toNovDecs :: T r -> NovDecs.T
toNovDecs (T nd sd) = nd `NovDecs.appendDecems`
  (SeqDecs.toDecems . SeqDecsNE.toSeqDecs $ sd)

numDigits :: T a -> NonZero.T
numDigits (T nd sd) = NovDecs.numDigits nd
  `NonZero.add` SeqDecsNE.numDigits sd
