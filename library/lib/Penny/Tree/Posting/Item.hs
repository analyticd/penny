module Penny.Tree.Posting.Item where

import qualified Penny.Tree.Flag as Flag
import qualified Penny.Tree.Number as Number
import qualified Penny.Tree.Payee.Posting as Payee
import qualified Penny.Tree.Account.Unquoted as Account.Unquoted
import qualified Penny.Tree.Account.Quoted as Account.Quoted
import qualified Penny.Tree.Tag as Tag
import qualified Penny.Tree.Side as Side

-- There is no need to import Currency; any Currency that is alone
-- will be parsed as an Ingot
import qualified Penny.Tree.Commodity as Commodity
import qualified Penny.Tree.Ingot as Ingot
import Control.Applicative
import Text.Parsec.Text

data T
  = Flag Flag.T
  | Number Number.T
  | Payee Payee.T
  | Unquoted Account.Unquoted.T
  | Quoted Account.Quoted.T
  | Tag Tag.T
  | Side Side.T
  | Commodity Commodity.T
  | Ingot Ingot.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Flag <$> Flag.parser
  <|> Number <$> Number.parser
  <|> Payee <$> Payee.parser
  <|> Unquoted <$> Account.Unquoted.parser
  <|> Quoted <$> Account.Quoted.parser
  <|> Tag <$> Tag.parser
  <|> Side <$> Side.parser
  <|> Commodity <$> Commodity.parser
  <|> Ingot <$> Ingot.parser
