module Penny.Core.NovDecs where

import Deka.Native.Abstract
import qualified Penny.Core.Decems as Decems
import qualified Penny.Core.DecDecs as DecDecs
import qualified Penny.Natural.NonZero as NonZero
import qualified Deka.Native as DN
import qualified Penny.Tree.Parsec as P
import Text.Parsec.Text
import Control.Applicative

data T = T
  { novem :: Novem
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

toNonZero :: T -> NonZero.T
toNonZero (T n d) =
  NonZero.addUnsigned (NonZero.fromNovem n) (Decems.toUnsigned d)

toDecuple :: T -> DN.Decuple
toDecuple (T n ds) = DN.Decuple n (Decems.toList ds)

fromDecuple :: DN.Decuple -> T
fromDecuple (DN.Decuple nv ds) = T nv (Decems.fromList ds)

parser :: Parser T
parser = T <$> P.novem <*> Decems.parser

toDecDecs :: T -> DecDecs.T
toDecDecs (T d1 ds) = DecDecs.T (Nonem d1) ds
