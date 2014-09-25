module Penny.Tree.Ingot.Comma where

import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna.RadCom as RadCom
import qualified Penny.Tree.Currency as Currency
import qualified Penny.Tree.Apostrophe as Apostrophe
import Control.Applicative
import Text.Parsec.Text

data T
  = T Apostrophe.T
      (Either (Currency.T, Lewis.T RadCom.T)
              (Lewis.T RadCom.T, Maybe Currency.T))
      Apostrophe.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = T
  <$> Apostrophe.parser
  <*> ( fmap Left ((,) <$> Currency.parser
                       <*> Lewis.parser RadCom.parseRadix RadCom.parser)
        <|> fmap Right ( (,) <$> Lewis.parser
                                 RadCom.parseRadix RadCom.parser
                             <*> optional Currency.parser))
  <*> Apostrophe.parser
