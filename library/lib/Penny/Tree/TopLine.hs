module Penny.Tree.TopLine where

import qualified Penny.Tree.Payee.TopLine as Payee
import qualified Penny.Tree.Newline as Newline
import Data.Sequence (Seq)
import qualified Penny.Tree.PostSpace as PostSpace
import qualified Penny.Tree.TopLine.Item as Item
import Control.Applicative
import Text.Parsec.Text
import qualified Data.Sequence as S
import qualified Penny.Tree.TopLine.Error as Error
import qualified Penny.Core.Memo as Memo
import qualified Penny.Core.Location as Location
import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Penny.Core.TopLine as TopLine
import qualified Penny.Tree.TopLine.Mayfield as Mayfield

data T = T (Seq (PostSpace.T Item.T)) (Maybe Payee.T) Newline.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = T
  <$> fmap S.fromList (many (PostSpace.parser Item.parser))
  <*> optional Payee.parser
  <*> Newline.parser

toCore
  :: T
  -> Either Error.T ( Memo.T
                      -> Location.T
                      -> Clxn.T
                      -> Global.T
                      -> Local.T
                      -> TopLine.T )
toCore (T sq pye _) = fmap ($ (fmap Payee.toCore pye))
  . Mayfield.toCore . fmap PostSpace.payload $ sq
