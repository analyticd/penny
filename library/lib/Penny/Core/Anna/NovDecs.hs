module Penny.Core.Anna.NovDecs where

import Deka.Native.Abstract
import qualified Penny.Core.Anna.Decems as Decems
import qualified Penny.Natural.NonZero as NonZero
import qualified Deka.Native as DN

data T = T
  { novem :: Novem
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

toNonZero :: T -> NonZero.T
toNonZero (T n d) = NonZero.addUnsigned (NonZero.fromNovem n) (Decems.toUnsigned d)

toDecuple :: T -> DN.Decuple
toDecuple (T n ds) = DN.Decuple n (Decems.toList ds)

fromDecuple :: DN.Decuple -> T
fromDecuple (DN.Decuple nv ds) = T nv (Decems.fromList ds)
