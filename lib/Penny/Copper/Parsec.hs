module Penny.Copper.Parsec where

import qualified Penny.Copper.Terminals as T
import Text.Parsec.Text (Parser)
import Text.Parsec (many, many1, satisfy)
import Control.Applicative ((<$>), (<$), (<*>), (*>), (<*),
                            (<|>))
import qualified Penny.Lincoln as L
import Data.Text (pack)

lvl1SubAcct :: Parser L.SubAccount
lvl1SubAcct =
  (L.SubAccount . pack) <$> many1 (satisfy T.lvl1AcctChar)

lvl1FirstSubAcct :: Parser L.SubAccount
lvl1FirstSubAcct = lvl1SubAcct

lvl1OtherSubAcct :: Parser L.SubAccount
lvl1OtherSubAcct = satisfy T.colon *> lvl1SubAcct

lvl1Acct :: Parser L.Account
lvl1Acct = f <$> lvl1FirstSubAcct <*> many lvl1OtherSubAcct
  where
    f a as = L.Account (a:as)

quotedLvl1Acct :: Parser L.Account
quotedLvl1Acct =
  satisfy T.openCurly *> lvl1Acct <* satisfy T.closeCurly

lvl2FirstSubAcct :: Parser L.SubAccount
lvl2FirstSubAcct =
  (\c cs -> L.SubAccount (pack (c:cs)))
  <$> satisfy T.letter
  <*> many (satisfy T.lvl2AcctOtherChar)

lvl2OtherSubAcct :: Parser L.SubAccount
lvl2OtherSubAcct =
  (L.SubAccount . pack)
  <$ satisfy T.colon
  <*> many1 (satisfy T.lvl2AcctOtherChar)

lvl2Acct :: Parser L.Account
lvl2Acct =
  (\a as -> L.Account (a:as))
  <$> lvl2FirstSubAcct
  <*> many lvl2OtherSubAcct

ledgerAcct :: Parser L.Account
ledgerAcct = quotedLvl1Acct <|> lvl2Acct

lvl1Cmdty :: Parser L.Commodity
lvl1Cmdty = (L.Commodity . pack) <$> many1 (satisfy T.lvl1CmdtyChar)

quotedLvl1Cmdty :: Parser L.Commodity
quotedLvl1Cmdty =
  satisfy (T.doubleQuote) *> lvl1Cmdty <* satisfy (T.doubleQuote)

lvl2Cmdty :: Parser L.Commodity
lvl2Cmdty =
  (\c cs -> L.Commodity (pack (c:cs)))
  <$> satisfy T.lvl2CmdtyFirstChar
  <*> many (satisfy T.lvl2CmdtyOtherChar)

lvl3Cmdty :: Parser L.Commodity
lvl3Cmdty = (L.Commodity . pack) <$> many1 (satisfy T.lvl3CmdtyChar)


