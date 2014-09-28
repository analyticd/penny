module Penny.Tree.Posting where

import qualified Penny.Tree.Posting.Item as Item
import qualified Penny.Tree.PreSpace as PreSpace
import qualified Penny.Tree.Newline as Newline
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Tree.Posting.Mayfield as Mayfield
import qualified Penny.Tree.Posting.Error as Error
import qualified Penny.Core.Memo as Memo
import qualified Penny.Core.Location as Location
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Penny.Core.Posting as Posting

data T = T (PreSpace.T Item.T) (Seq (PreSpace.T Item.T)) Newline.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = T
  <$> PreSpace.parser Item.parser
  <*> fmap S.fromList (many (PreSpace.parser Item.parser))
  <*> Newline.parser

harvest :: T -> Either Error.T ( Memo.T
                                 -> Location.T
                                 -> Global.T
                                 -> Local.T
                                 -> Posting.T )
harvest (T _ seqnce _)
  = Mayfield.procItems (fmap PreSpace.payload seqnce)
