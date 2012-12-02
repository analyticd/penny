-- | The Attoparsec parser for the Copper file format. Because
-- Attoparsec does not track locations, this parser does not provide
-- line number metadata. It would be possible to write an Attoparsec
-- parser which provides line number information. This would require
-- doing the parse in two phases--one Attoparsec phase that returns a
-- list of data, with one atom for each line, and a second phase that
-- folds over this list to create the actual items. This would be
-- possible, but rather nasty to write and currently not worth the
-- effort.
--
-- At first I tried using a state transformer monad to keep track of
-- the line number, but it's not clear how to handle
-- backtracking--that is, attoparsec might backtrack, but it's not
-- going to rewind the outer state transformer monad. Also, you cannot
-- use combinators like many on a state transformer monad, because
-- unlike the parser it's not a member of Alternative. Maybe I could
-- write a custom monad to wrap Attoparsec that would also be a member
-- of Alterntive? But I'm not sure that can be done while respecting
-- the laws of the various typeclasses and, besides, by the time you
-- do all that...might as well use Parsec, which has already done it :)
module Penny.Copper.Atto where

import Control.Applicative.Permutation as AP
import Control.Applicative
  ((<$>), (<*>), (*>), (<*), (<$), many, (<|>), optional)
import Control.Monad (replicateM)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Attoparsec.Text (Parser, skip, satisfy)
import qualified Data.Attoparsec.Text as P
import Data.Maybe (fromMaybe)
import qualified Data.Time as Time
import qualified Penny.Copper.Terminals as T
import qualified Penny.Copper.Types as Y
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import Data.Text (Text, unpack)
import qualified Penny.Lincoln.Transaction.Unverified as U

-- * Accounts

lvl1SubAcct :: Parser L.SubAccount
lvl1SubAcct = L.SubAccount <$> P.takeWhile1 T.lvl1AcctChar

lvl1FirstSubAcct :: Parser L.SubAccount
lvl1FirstSubAcct = lvl1SubAcct

lvl1OtherSubAcct :: Parser L.SubAccount
lvl1OtherSubAcct = skip T.colon *> lvl1SubAcct

lvl1Acct :: Parser L.Account
lvl1Acct = (\a1 as -> L.Account (a1:as))
           <$> lvl1FirstSubAcct <*> many lvl1OtherSubAcct

quotedLvl1Acct :: Parser L.Account
quotedLvl1Acct = skip T.openCurly *> lvl1Acct <* skip T.closeCurly

lvl2FirstSubAcct :: Parser L.SubAccount
lvl2FirstSubAcct =
  (\c1 cs -> L.SubAccount (c1 `X.cons` cs))
  <$> satisfy T.letter
  <*> P.takeWhile T.lvl2AcctOtherChar

lvl2OtherSubAcct :: Parser L.SubAccount
lvl2OtherSubAcct =
  L.SubAccount <$ skip T.colon <*> P.takeWhile1 T.lvl2AcctOtherChar

lvl2Acct :: Parser L.Account
lvl2Acct = (\a1 as -> L.Account (a1:as))
           <$> lvl2FirstSubAcct <*> many lvl2OtherSubAcct

ledgerAcct :: Parser L.Account
ledgerAcct = quotedLvl1Acct <|> lvl2Acct

-- * Commodities

lvl1Cmdty :: Parser L.Commodity
lvl1Cmdty = L.Commodity <$> P.takeWhile1 T.lvl1CmdtyChar

quotedLvl1Cmdty :: Parser L.Commodity
quotedLvl1Cmdty =
  skip T.doubleQuote *> lvl1Cmdty <* skip T.doubleQuote

lvl2Cmdty :: Parser L.Commodity
lvl2Cmdty =
  (\c cs -> L.Commodity (c `X.cons` cs))
  <$> satisfy T.lvl2CmdtyFirstChar
  <*> P.takeWhile T.lvl2CmdtyOtherChar

lvl3Cmdty :: Parser L.Commodity
lvl3Cmdty = L.Commodity <$> P.takeWhile1 T.lvl3CmdtyChar

digitGroup :: Parser Text
digitGroup = skip T.thinSpace *> P.takeWhile1 T.digit

digitSequence :: Parser Text
digitSequence =
  (\ds dss -> X.append ds (X.concat dss))
  <$> P.takeWhile1 T.digit
  <*> many digitGroup

digitPostSequence :: Parser (Maybe Text)
digitPostSequence =
  skip T.period *> optional digitSequence

quantity :: Parser L.Qty
quantity = p >>= failOnErr
  where
    p = radFrac <|> mkWhole
    radFrac = (L.RadFrac . unpack) <$> (skip T.period *> digitSequence)
    mkWhole = f <$> digitSequence <*> optional digitPostSequence
      where
        f digSeq maybePostSeq = case maybePostSeq of
          Nothing -> L.Whole . unpack $ digSeq
          Just ps ->
            maybe (L.WholeRad . unpack $ digSeq)
                  (L.WholeRadFrac . unpack $ digSeq) (fmap unpack ps)
    failOnErr = maybe (fail msg) return . L.toQty
    msg = "could not read quantity; zero quantities not allowed"

spaceBetween :: Parser L.SpaceBetween
spaceBetween = f <$> optional (P.takeWhile1 T.white)
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

leftSideCmdtyAmt :: Parser (L.Amount, L.Format)
leftSideCmdtyAmt = leftCmdtyLvl1Amt <|> leftCmdtyLvl3Amt

rightSideCmdty :: Parser L.Commodity
rightSideCmdty = quotedLvl1Cmdty <|> lvl2Cmdty

rightSideCmdtyAmt :: Parser (L.Amount, L.Format)
rightSideCmdtyAmt =
  f <$> quantity <*> spaceBetween <*> rightSideCmdty
  where
    f q s c = (L.Amount q c, L.Format L.CommodityOnRight s)


amount :: Parser (L.Amount, L.Format)
amount = leftSideCmdtyAmt <|> rightSideCmdtyAmt

skipWhite :: Parser ()
skipWhite = P.skipWhile T.white

comment :: Parser Y.Comment
comment = Y.Comment <$ skip T.hash <*> P.takeWhile T.nonNewline
          <* skip T.newline <* P.skipWhile T.white

year :: Parser Integer
year = read <$> replicateM 4 (P.satisfy T.digit)

month :: Parser Int
month = read <$> replicateM 2 (P.satisfy T.digit)

day :: Parser Int
day = read <$> replicateM 2 (P.satisfy T.digit)

date :: Parser Time.Day
date = p >>= failOnErr
  where
    p = Time.fromGregorianValid
        <$> year  <* skip T.dateSep
        <*> month <* skip T.dateSep
        <*> day
    failOnErr = maybe (fail "could not parse date") return


hours :: Parser L.Hours
hours = p >>= (maybe (fail "could not parse hours") return)
  where
    p = f <$> satisfy T.digit <*> satisfy T.digit
    f d1 d2 = L.intToHours . read $ [d1,d2]


minutes :: Parser L.Minutes
minutes = p >>= maybe (fail "could not parse minutes") return
  where
    p = f <$ skip T.colon <*> satisfy T.digit <*> satisfy T.digit
    f d1 d2 = L.intToMinutes . read $ [d1, d2]

seconds :: Parser L.Seconds
seconds = p >>= maybe (fail "could not parse seconds") return
  where
    p = f <$ skip T.colon <*> satisfy T.digit <*> satisfy T.digit
    f d1 d2 = L.intToSeconds . read $ [d1, d2]

time :: Parser (L.Hours, L.Minutes, Maybe L.Seconds)
time = (,,) <$> hours <*> minutes <*> optional seconds

tzSign :: Parser (Int -> Int)
tzSign = (id <$ skip T.plus) <|> (negate <$ skip T.minus)

tzNumber :: Parser Int
tzNumber = read <$> replicateM 4 (satisfy T.digit)

timeZone :: Parser L.TimeZoneOffset
timeZone = p >>= maybe (fail "could not parse time zone") return
  where
    p = f <$> tzSign <*> tzNumber
    f s = L.minsToOffset . s

timeWithZone
  :: Parser (L.Hours, L.Minutes,
             Maybe L.Seconds, Maybe L.TimeZoneOffset)
timeWithZone =
  f <$> time <* skipWhite <*> optional timeZone
  where
    f (h, m, s) tz = (h, m, s, tz)

dateTime :: Parser L.DateTime
dateTime =
  f <$> date <* skipWhite <*> optional timeWithZone
  where
    f d mayTwithZ = L.DateTime d h m s tz
      where
        ((h, m, s), tz) = case mayTwithZ of
          Nothing -> (L.midnight, L.noOffset)
          Just (hr, mn, mayS, mayTz) ->
            let sec = fromMaybe L.zeroSeconds mayS
                z = fromMaybe L.noOffset mayTz
            in ((hr, mn, sec), z)

debit :: Parser L.DrCr
debit = L.Debit <$ skip T.lessThan

credit :: Parser L.DrCr
credit = L.Credit <$ skip T.greaterThan

drCr :: Parser L.DrCr
drCr = debit <|> credit

entry :: Parser (L.Entry, L.Format)
entry = f <$> drCr <* skipWhite <*> amount
  where
    f dc (am, fmt) = (L.Entry dc am, fmt)

flag :: Parser L.Flag
flag = L.Flag <$ skip T.openSquare
  <*> P.takeWhile T.flagChar <* skip T.closeSquare

postingMemoLine :: Parser Text
postingMemoLine =
  skip T.apostrophe
  *> P.takeWhile T.nonNewline
  <* skip T.newline <* skipWhite

postingMemo :: Parser L.Memo
postingMemo = L.Memo <$> P.many1 postingMemoLine


transactionMemoLine :: Parser Text
transactionMemoLine =
  skip T.semicolon *> P.takeWhile T.nonNewline
  <* skip T.newline <* skipWhite

transactionMemo :: Parser L.Memo
transactionMemo = L.Memo <$> P.many1 transactionMemoLine

number :: Parser L.Number
number =
  L.Number <$ skip T.openParen
  <*> P.takeWhile T.numberChar <* skip T.closeParen

lvl1Payee :: Parser L.Payee
lvl1Payee = L.Payee <$> P.takeWhile T.quotedPayeeChar

quotedLvl1Payee :: Parser L.Payee
quotedLvl1Payee = skip T.tilde *> lvl1Payee <* skip T.tilde

lvl2Payee :: Parser L.Payee
lvl2Payee = (\c cs -> L.Payee (X.cons c cs)) <$> satisfy T.letter
            <*> P.takeWhile T.nonNewline

fromCmdty :: Parser L.From
fromCmdty = L.From <$> (quotedLvl1Cmdty <|> lvl2Cmdty)

price :: Parser L.PricePoint
price = p >>= maybe (fail msg) return
  where
    f dt fr ((L.Amount qt to), fmt) =
      let cpu = L.CountPerUnit qt
      in case L.newPrice fr (L.To to) cpu of
        Nothing -> Nothing
        Just pr ->
          let pmt = L.PriceMeta Nothing (Just fmt)
          in Just $ L.PricePoint dt pr pmt
    p = f <$ satisfy T.atSign <* skipWhite
        <*> dateTime <* skipWhite
        <*> fromCmdty <* skipWhite
        <*> amount <* satisfy T.newline <* skipWhite
    msg = "could not parse price, make sure the from and to commodities "
          ++ "are different"

tag :: Parser L.Tag
tag = L.Tag <$ skip T.asterisk <*> P.takeWhile T.tagChar
      <* skipWhite

tags :: Parser L.Tags
tags = (\t ts -> L.Tags (t:ts)) <$> tag <*> many tag

topLinePayee :: Parser L.Payee
topLinePayee = quotedLvl1Payee <|> lvl2Payee

topLineFlagNum :: Parser (Maybe L.Flag, Maybe L.Number)
topLineFlagNum = p1 <|> p2
  where
    p1 = ( (,) <$> optional flag
               <* skipWhite <*> optional number)
    p2 = ( flip (,)
           <$> optional number
           <* skipWhite <*> optional flag)

topLine :: Parser U.TopLine
topLine =
  f <$> optional transactionMemo
    <*> dateTime
    <*  skipWhite
    <*> topLineFlagNum
    <*  skipWhite
    <*> optional topLinePayee
    <*  satisfy T.newline
    <*  skipWhite
  where
    f mayMe dt (mayFl, mayNum) mayPy =
      U.TopLine dt mayFl mayNum mayPy mayMe mt
      where
        mt = L.TopLineMeta Nothing Nothing Nothing Nothing Nothing

flagNumPayee :: Parser (Maybe L.Flag, Maybe L.Number, Maybe L.Payee)
flagNumPayee =
  AP.runPerms
  $ (,,)
  <$> AP.maybeAtom (flag <* skipWhite)
  <*> AP.maybeAtom (number <* skipWhite)
  <*> AP.maybeAtom (quotedLvl1Payee <* skipWhite)

postingAcct :: Parser L.Account
postingAcct = quotedLvl1Acct <|> lvl2Acct

posting :: Parser U.Posting
posting = f <$> flagNumPayee           <* skipWhite
            <*> postingAcct            <* skipWhite
            <*> optional tags          <* skipWhite
            <*> optional entry         <* skipWhite
            <*  skip T.newline         <* skipWhite
            <*> optional postingMemo   <* skipWhite
  where
    f (fl, nu, pa) ac ta enPair me =
      U.Posting pa nu fl ac tgs en me mt
      where
        tgs = fromMaybe (L.Tags []) ta
        en = fmap fst enPair
        mt = L.PostingMeta Nothing fmt Nothing Nothing
        fmt = fmap snd enPair

transaction :: Parser L.Transaction
transaction = p >>= Ex.switch (fail . show) return
  where
    p = L.transaction <$>
        (L.Family <$> topLine <*> posting
        <*> posting <*> many posting)


blankLine :: Parser Y.Item
blankLine = Y.BlankLine <$ skip T.newline <* skipWhite

item :: Parser Y.Item
item = fmap Y.IComment comment <|> fmap Y.PricePoint price
       <|> fmap Y.Transaction transaction <|> blankLine

ledger :: Parser Y.Ledger
ledger = Y.Ledger <$ skipWhite <*> many item <* P.endOfInput
