module Penny.Parser.Account where

import Control.Monad ( liftM, void )
import Data.Char ( isLetter, isNumber )
import Data.Text ( pack )
import Text.Parsec (
  char, satisfy, notFollowedBy, (<|>), try, many )
import Text.Parsec.Text ( Parser )

import Penny.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ) )
import qualified Penny.Bits as B
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

subAccountChar :: Parser Char
subAccountChar = let
  notSpc = satisfy (\l -> isLetter l || isNumber l)
  spc = do
    void $ char ' '
    notFollowedBy (char ' ' <|> char '#')
    return ' '
  in notSpc <|> try spc

subAccountName :: Parser B.SubAccountName
subAccountName = do
  c <- subAccountChar
  r <- liftM pack $ many subAccountChar
  return . B.SubAccountName $ TextNonEmpty c r

firstSubAccount :: Parser B.SubAccountName
firstSubAccount = subAccountName

nextSubAccount :: Parser B.SubAccountName
nextSubAccount = char ':' >> subAccountName

account :: Parser B.Account
account = do
  f <- firstSubAccount
  r <- many nextSubAccount
  return . B.Account $ AtLeast1 f r

