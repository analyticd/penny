module Penny.Copper.Parsec where

import qualified Penny.Copper.Terminals as T
import Text.Parsec.Text (Parser)
import Text.Parsec (many, many1, satisfy)
import Control.Applicative ((<$>), (<$), (<*>), (*>), (<*),
                            (<|>), optional)
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
  satisfy T.doubleQuote *> lvl1Cmdty <* satisfy (T.doubleQuote)

lvl2Cmdty :: Parser L.Commodity
lvl2Cmdty =
  (\c cs -> L.Commodity (pack (c:cs)))
  <$> satisfy T.lvl2CmdtyFirstChar
  <*> many (satisfy T.lvl2CmdtyOtherChar)

lvl3Cmdty :: Parser L.Commodity
lvl3Cmdty = (L.Commodity . pack) <$> many1 (satisfy T.lvl3CmdtyChar)

digitGroup :: Parser [Char]
digitGroup = satisfy T.thinSpace *> many1 (satisfy T.digit)

digitSequence :: Parser [Char]
digitSequence =
  (++) <$> many1 (satisfy T.digit)
  <*> (concat <$> (many digitGroup))

digitPostSequence :: Parser (Maybe [Char])
digitPostSequence = satisfy T.period *> optional digitSequence

quantity :: Parser L.Qty
quantity = p >>= failOnErr
  where
    p = (L.Whole <$> (satisfy T.period *> digitSequence))
        <|> (f <$> digitSequence <*> optional digitPostSequence)
    f digSeq maybePostSeq = case maybePostSeq of
      Nothing -> L.Whole digSeq
      Just ps ->
        maybe (L.WholeRad digSeq) (L.WholeRadFrac digSeq) ps
    failOnErr = maybe (fail msg) return . L.toQty
    msg = "could not read quantity; zero quantities not allowed"

spaceBetween :: Parser L.SpaceBetween
spaceBetween = f <$> optional (many1 (satisfy T.white))
  where
    f = maybe L.NoSpaceBetween (const L.SpaceBetween)

leftCmdtyLvl1Amt :: Parser (L.Amount, L.Format)
leftCmdtyLvl1Amt =
  f <$> quotedLvl1Cmdty <*> spaceBetween <*> quantity
  where
    f c s q = (L.Amount q c, L.Format L.CommodityOnLeft s)

leftCmdtyLvl3Amt :: Parser (L.Amount, L.Format)
leftCmdtyLvl3Amt = f <$> lvl3Cmdty <*> spaceBetween <*> quantity
  where
    f c s q = (L.Amount q c, L.Format L.CommodityOnLeft s)

rightCmdtyLvl1Amt :: Parser (L.Amount, L.Format)
rightCmdtyLvl1Amt =
  f <$> quantity <*> spaceBetween <*> quotedLvl1Cmdty
  where
    f q s c = (L.Amount q c, L.Format L.CommodityOnRight s)

rightCmdtyLvl2Amt :: Parser (L.Amount, L.Format)
rightCmdtyLvl2Amt =
  f <$> quantity <*> spaceBetween <*> lvl2Cmdty
  where
    f q s c = (L.Amount q c, L.Format L.CommodityOnRight s)

leftSideCmdtyAmt :: Parser (L.Amount, L.Format)
leftSideCmdtyAmt = leftCmdtyLvl1Amt <|> leftCmdtyLvl3Amt

rightSideCmdtyAmt :: Parser (L.Amount, L.Format)
rightSideCmdtyAmt = rightCmdtyLvl1Amt <|> rightCmdtyLvl2Amt

amount :: Parser (L.Amount, L.Format)
amount = leftSideCmdtyAmt <|> rightSideCmdtyAmt

comment :: 
