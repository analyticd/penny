module Penny.Core.Anna.DecDecsMayGroups where

import qualified Penny.Core.DecDecs as DecDecs
import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.Decems as Decems
import qualified Penny.Natural.NonZero as NonZero
import Data.Monoid

data T r = T
  { first :: DecDecs.T
  , rest :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

toDecems :: T a -> Decems.T
toDecems (T f r) = DecDecs.toDecems f <> SeqDecs.toDecems r

numDigits :: T a -> NonZero.T
numDigits (T f r) = DecDecs.numDigits f `NonZero.addUnsigned`
  (SeqDecs.numDigits r)
