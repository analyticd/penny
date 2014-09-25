module Penny.Tree.Price.Price3 where

import qualified Penny.Tree.Square.Open as Open
import qualified Penny.Tree.Square.Close as Close
import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna.RadPer as RadPer
import qualified Penny.Core.Anna.RadCom as RadCom
import qualified Penny.Tree.Spaces as Spaces
import Text.Parsec.Text
import Control.Applicative

-- | Occurs after a To commodity.

data T
  = Square Open.T (Maybe Spaces.T)
           (Lewis.T RadCom.T) (Maybe Spaces.T) Close.T
  -- ^ An opening square brace, followed by a comma-radixed to amount
  | Unquoted (Lewis.T RadPer.T)
  -- ^ An unquoted period-radixed to amount
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Square <$> Open.parser <*> optional Spaces.parser
           <*> Lewis.parser RadCom.parseRadix RadCom.parser
           <*> optional Spaces.parser <*> Close.parser

  <|> Unquoted <$> Lewis.parser RadPer.parseRadix RadPer.parser
