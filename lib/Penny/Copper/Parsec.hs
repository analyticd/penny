-- | Parsec parsers for the ledger file format. The format is
-- documented in EBNF in the file @doc\/ledger-grammar.org@.
module Penny.Copper.Parsec where

-- # Imports

import qualified Penny.Copper.Interface as I
import qualified Penny.Copper.Terminals as T
import Text.Parsec.Text (Parser)
import Text.Parsec (many, many1, satisfy, (<?>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as Pos
import Control.Applicative.Permutation (runPerms, maybeAtom)
import Control.Applicative ((<$>), (<$), (<*>), (*>), (<*),
                            (<|>), optional)
import Control.Monad (replicateM, when)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Penny.Lincoln as L
import qualified Penny.Steel.Sums as S
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified System.Exit as Exit
import System.Environment (getProgName)
import qualified System.IO as IO
import qualified Data.Text.IO as TIO

-- # Helpers

nonEmpty :: Parser a -> Parser (NonEmpty a)
nonEmpty p = (:|) <$> p <*> many p

-- # Accounts

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

-- # Commodities

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

-- # Quantities

digit :: Parser L.Digit
digit = (P.choice . map f $ zip ['0'..'9'] [minBound..maxBound])
  <?> "digit"
  where
    f (s, d) = d <$ P.char s

digitList :: Parser L.DigitList
digitList = L.DigitList <$> nonEmpty digit

groupPart :: Parser a -> Parser (a, L.DigitList)
groupPart p = (,) <$> p <*> digitList

groupedDigits :: Parser a -> Parser (L.GroupedDigits a)
groupedDigits p
  = L.GroupedDigits <$> digitList <*> many (groupPart p)

-- | Parses a sequence of grouped digits, followed by an optional
-- radix point, followed by an optional additional sequence of grouped
-- digits.  Numbers such as .25 are not allowed; instead,
-- the user must enter 0.25. Also not allowed is something like
-- "25.". Intsead, if the user enters a radix, there must be a
-- character after it.
digitsRadDigits
  :: Parser a
  -- ^ Parses a single grouping character
  -> Parser void
  -- ^ Parses a radix point
  -> Parser (L.GroupedDigits a, Maybe (L.GroupedDigits a))
digitsRadDigits gc r = do
  g1 <- groupedDigits gc
  maybeRad <- optional r
  case maybeRad of
    Nothing -> return (g1, Nothing)
    Just _ -> do
      g2 <- groupedDigits gc
      return (g1, Just g2)

-- | Parses an unquoted QtyRep.
unquotedQtyRep :: Parser L.QtyRep
unquotedQtyRep = do
  let gc = P.choice [ L.PGThinSpace <$ P.char '\x2009'
                    , L.PGComma <$ P.char ',' ]
      r = P.char '.'
  (g1, mayg2) <- digitsRadDigits gc r
  case L.wholeOrFrac g1 mayg2 of
    Nothing -> fail "failed to parse quantity"
    Just ei -> return . L.wholeOrFracToQtyRep . Left $ ei

-- | Parses an unquoted QtyRep that also has spaces. Use only when
-- parsing command line items.
unquotedQtyRepWithSpaces :: Parser L.QtyRep
unquotedQtyRepWithSpaces = do
  let gc = P.choice [ L.PGThinSpace <$ P.char '\x2009'
                    , L.PGComma <$ P.char ','
                    , L.PGSpace <$ P.char ' ' ]
      r = P.char '.'
  (g1, mayg2) <- digitsRadDigits gc r
  case L.wholeOrFrac g1 mayg2 of
    Nothing -> fail "failed to parse quantity"
    Just ei -> return . L.wholeOrFracToQtyRep . Left $ ei

-- | Parses a QtyRep that is quoted with square braces. This is a
-- QtyRep that uses a comma as the radix point.
quotedCommaQtyRep :: Parser L.QtyRep
quotedCommaQtyRep = do
  let gc = P.choice [ L.CGThinSpace <$ P.char '\x2009'
                    , L.CGSpace <$ P.char ' '
                    , L.CGPeriod <$ P.char '.' ]
      r = P.char ','
  _ <- P.char '['
  (g1, mayg2) <- digitsRadDigits gc r
  _ <- P.char ']'
  case L.wholeOrFrac g1 mayg2 of
    Nothing -> fail "failed to parse quantity"
    Just ei -> return . L.wholeOrFracToQtyRep . Right $ ei

-- | Parses a QtyRep that is quoted with curly braces. This is a
-- QtyRep that uses a period as the radix point. Unlike an unquoted
-- QtyRep this can include spaces.
quotedPeriodQtyRep :: Parser L.QtyRep
quotedPeriodQtyRep = do
  let gc = P.choice [ L.PGThinSpace <$ P.char '\x2009'
                    , L.PGComma <$ P.char ','
                    , L.PGSpace <$ P.char ' '
                    ]
      r = P.char '.'
  _ <- P.char '{'
  (g1, mayg2) <- digitsRadDigits gc r
  _ <- P.char '}'
  case L.wholeOrFrac g1 mayg2 of
    Nothing -> fail "failed to parse quantity"
    Just ei -> return . L.wholeOrFracToQtyRep . Left $ ei

qtyRep :: Parser L.QtyRep
qtyRep = unquotedQtyRep <|> quotedPeriodQtyRep <|> quotedCommaQtyRep
         <?> "quantity"

-- # Amounts

spaceBetween :: Parser L.SpaceBetween
spaceBetween = f <$> optional (many1 (satisfy T.white))
  where
    f = maybe L.NoSpaceBetween (const L.SpaceBetween)

leftCmdtyLvl1Amt :: Parser (L.Amount L.QtyRep, L.Side, L.SpaceBetween)
leftCmdtyLvl1Amt =
  f <$> quotedLvl1Cmdty <*> spaceBetween <*> qtyRep
  where
    f c s q = (L.Amount q c , L.CommodityOnLeft, s)

leftCmdtyLvl3Amt :: Parser (L.Amount L.QtyRep, L.Side, L.SpaceBetween)
leftCmdtyLvl3Amt = f <$> lvl3Cmdty <*> spaceBetween <*> qtyRep
  where
    f c s q = (L.Amount q c, L.CommodityOnLeft, s)

leftSideCmdtyAmt :: Parser (L.Amount L.QtyRep, L.Side, L.SpaceBetween)
leftSideCmdtyAmt = leftCmdtyLvl1Amt <|> leftCmdtyLvl3Amt

rightSideCmdty :: Parser L.Commodity
rightSideCmdty = quotedLvl1Cmdty <|> lvl2Cmdty

rightSideCmdtyAmt :: Parser (L.Amount L.QtyRep, L.Side, L.SpaceBetween)
rightSideCmdtyAmt =
  f <$> qtyRep <*> spaceBetween <*> rightSideCmdty
  where
    f q s c = (L.Amount q c, L.CommodityOnRight, s)


amount :: Parser (L.Amount L.QtyRep, L.Side, L.SpaceBetween)
amount = leftSideCmdtyAmt <|> rightSideCmdtyAmt

-- # Comments

comment :: Parser I.Comment
comment =
  (I.Comment . pack)
  <$ satisfy T.hash
  <*> many (satisfy T.nonNewline)
  <* satisfy T.newline
  <* many (satisfy T.white)

-- # Dates and times

year :: Parser Integer
year = read <$> replicateM 4 P.digit

month :: Parser Int
month = read <$> replicateM 2 P.digit

day :: Parser Int
day = read <$> replicateM 2 P.digit

date :: Parser Time.Day
date = p >>= failOnErr
  where
    p = Time.fromGregorianValid
        <$> year  <* satisfy T.dateSep
        <*> month <* satisfy T.dateSep
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
    p = f <$ satisfy T.colon <*> satisfy T.digit <*> satisfy T.digit
    f d1 d2 = L.intToMinutes . read $ [d1, d2]

seconds :: Parser L.Seconds
seconds = p >>= maybe (fail "could not parse seconds") return
  where
    p = f <$ satisfy T.colon <*> satisfy T.digit <*> satisfy T.digit
    f d1 d2 = L.intToSeconds . read $ [d1, d2]

time :: Parser (L.Hours, L.Minutes, Maybe L.Seconds)
time = (,,) <$> hours <*> minutes <*> optional seconds

tzSign :: Parser (Int -> Int)
tzSign = (id <$ satisfy T.plus) <|> (negate <$ satisfy T.minus)

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
  f <$> time <* many (satisfy T.white) <*> optional timeZone
  where
    f (h, m, s) tz = (h, m, s, tz)

dateTime :: Parser L.DateTime
dateTime =
  f <$> date <* many (satisfy T.white) <*> optional timeWithZone
  where
    f d mayTwithZ = L.DateTime d h m s tz
      where
        ((h, m, s), tz) = case mayTwithZ of
          Nothing -> (L.midnight, L.noOffset)
          Just (hr, mn, mayS, mayTz) ->
            let sec = fromMaybe L.zeroSeconds mayS
                z = fromMaybe L.noOffset mayTz
            in ((hr, mn, sec), z)

-- # Debit and credit

debit :: Parser L.DrCr
debit = L.Debit <$ satisfy T.lessThan

credit :: Parser L.DrCr
credit = L.Credit <$ satisfy T.greaterThan

drCr :: Parser L.DrCr
drCr = debit <|> credit

-- # Entries

entry :: Parser (L.Entry L.QtyRep, L.Side, L.SpaceBetween)
entry = f <$> drCr <* (many (satisfy T.white)) <*> amount
  where
    f dc (am, sd, sb) = (L.Entry dc am, sd, sb)

-- # Flag

flag :: Parser L.Flag
flag = (L.Flag . pack) <$ satisfy T.openSquare
  <*> many (satisfy T.flagChar) <* satisfy (T.closeSquare)

-- # Memos

-- ## Posting memo

postingMemoLine :: Parser Text
postingMemoLine =
  pack
  <$ satisfy T.apostrophe
  <*> many (satisfy T.nonNewline)
  <* satisfy T.newline <* many (satisfy T.white)

postingMemo :: Parser L.Memo
postingMemo = L.Memo <$> many1 postingMemoLine

-- ## Transaction memo

transactionMemoLine :: Parser Text
transactionMemoLine =
  pack
  <$ satisfy T.semicolon <*> many (satisfy T.nonNewline)
  <* satisfy T.newline <* skipWhite

transactionMemo :: Parser (L.TopMemoLine, L.Memo)
transactionMemo = f <$> lineNum <*> many1 transactionMemoLine
  where
    f tml ls = (L.TopMemoLine tml
               , L.Memo ls)


-- # Number

number :: Parser L.Number
number =
  L.Number . pack <$ satisfy T.openParen
  <*> many (satisfy T.numberChar) <* satisfy T.closeParen

-- # Payees

lvl1Payee :: Parser L.Payee
lvl1Payee = L.Payee . pack <$> many (satisfy T.quotedPayeeChar)

quotedLvl1Payee :: Parser L.Payee
quotedLvl1Payee = satisfy T.tilde *> lvl1Payee <* satisfy T.tilde

lvl2Payee :: Parser L.Payee
lvl2Payee = (\c cs -> L.Payee (pack (c:cs))) <$> satisfy T.letter
            <*> many (satisfy T.nonNewline)

-- # Prices

fromCmdty :: Parser L.From
fromCmdty = L.From <$> (quotedLvl1Cmdty <|> lvl2Cmdty)

lineNum :: Parser Int
lineNum = Pos.sourceLine <$> P.getPosition

price :: Parser L.PricePoint
price = p >>= maybe (fail msg) return
  where
    f li dt fr (L.Amount qt to, sd, sb) =
      let cpu = L.CountPerUnit qt
      in case L.newPrice fr (L.To to) cpu of
        Nothing -> Nothing
        Just pr -> Just $ L.PricePoint dt pr
                          (Just sd) (Just sb) (Just $ L.PriceLine li)
    p = f <$> lineNum <* satisfy T.atSign <* skipWhite
        <*> dateTime <* skipWhite
        <*> fromCmdty <* skipWhite
        <*> amount <* satisfy T.newline <* skipWhite
    msg = "could not parse price, make sure the from and to commodities "
          ++ "are different"

-- # Tags

tag :: Parser L.Tag
tag = L.Tag . pack <$ satisfy T.asterisk <*> many (satisfy T.tagChar)
      <* many (satisfy T.white)

tags :: Parser L.Tags
tags = (\t ts -> L.Tags (t:ts)) <$> tag <*> many tag

-- # TopLine

topLinePayee :: Parser L.Payee
topLinePayee = quotedLvl1Payee <|> lvl2Payee

topLineFlagNum :: Parser (Maybe L.Flag, Maybe L.Number)
topLineFlagNum = p1 <|> p2
  where
    p1 = ( (,) <$> optional flag
               <* many (satisfy T.white) <*> optional number)
    p2 = ( flip (,)
           <$> optional number
           <* many (satisfy T.white) <*> optional flag)

skipWhite :: Parser ()
skipWhite = () <$ many (satisfy T.white)

topLine :: Parser I.ParsedTopLine
topLine =
  f <$> optional transactionMemo
    <*> lineNum
    <*> dateTime
    <*  skipWhite
    <*> topLineFlagNum
    <*  skipWhite
    <*> optional topLinePayee
    <*  satisfy T.newline
    <*  skipWhite
  where
    f mayMe lin dt (mayFl, mayNum) mayPy =
      I.ParsedTopLine dt mayNum mayFl mayPy me (L.TopLineLine lin)
      where
        me = fmap (\(a, b) -> (b, a)) mayMe

-- # Postings

flagNumPayee :: Parser (Maybe L.Flag, Maybe L.Number, Maybe L.Payee)
flagNumPayee = runPerms
  ( (,,) <$> maybeAtom (flag <* skipWhite)
         <*> maybeAtom (number <* skipWhite)
         <*> maybeAtom (quotedLvl1Payee <* skipWhite) )

postingAcct :: Parser L.Account
postingAcct = quotedLvl1Acct <|> lvl2Acct

posting :: Parser (L.PostingCore, L.PostingLine, Maybe (L.Entry L.QtyRep))
posting = f <$> lineNum                <* skipWhite
            <*> optional flagNumPayee  <* skipWhite
            <*> postingAcct            <* skipWhite
            <*> optional tags          <* skipWhite
            <*> optional entry         <* skipWhite
            <*  satisfy T.newline      <* skipWhite
            <*> optional postingMemo   <* skipWhite
  where
    f li mayFnp ac ta mayEn me =
      (L.PostingCore pa nu fl ac tgs me sd sb, pl, en)
      where
        tgs = fromMaybe (L.Tags []) ta
        pl = L.PostingLine li
        (fl, nu, pa) = fromMaybe (Nothing, Nothing, Nothing) mayFnp
        (en, sd, sb) = maybe (Nothing, Nothing, Nothing)
          (\(a, b, c) -> (Just a, Just b, Just c)) mayEn

-- # Transaction

transaction :: Parser I.ParsedTxn
transaction = do
  ptl <- topLine
  let getEntPair (core, lin, mayEn) = (fmap Left mayEn, (core, lin))
  ts <- fmap (map getEntPair) $ many posting
  ents <- maybe (fail "unbalanced transaction") return $ L.ents ts
  return (ptl, ents)


-- # Blank line

blankLine :: Parser ()
blankLine = () <$ satisfy T.newline <* skipWhite

-- # Item

item :: Parser I.ParsedItem
item
  = fmap S.S4a transaction
  <|> fmap S.S4b price
  <|> fmap S.S4c comment
  <|> (S.S4d I.BlankLine) <$ blankLine


-- # Parsing

parse
  :: Text
  -- ^ Contents of file to be parsed

  -> Ex.Exceptional String [I.ParsedItem]
  -- ^ Returns items if successfully parsed; otherwise, returns an
  -- error message.

parse s =
  let parser = P.spaces *> P.many item <* P.spaces <* P.eof
  in Ex.mapException show . Ex.fromEither
     $ P.parse parser "" s


getStdin :: IO Text
getStdin = do
  pn <- getProgName
  isTerm <- IO.hIsTerminalDevice IO.stdin
  when isTerm
       (IO.hPutStrLn IO.stderr $
        pn ++ ": warning: reading from standard input, which"
           ++ " is a terminal.")
  TIO.hGetContents IO.stdin


getFileContentsStdin :: String -> IO (L.Filename, Text)
getFileContentsStdin s = do
  txt <- if s == "-"
    then getStdin
    else TIO.readFile s
  let fn = L.Filename . X.pack $ if s == "-" then "<stdin>" else s
  return (fn, txt)


parseStdinOnly :: IO (L.Filename, [I.ParsedItem])
parseStdinOnly = do
  txt <- getStdin
  case parse txt of
    Ex.Exception err -> handleParseError "standard input" err
    Ex.Success g -> return (L.Filename . X.pack $ "<stdin>", g)

parseFromFilename :: String -> IO (L.Filename, [I.ParsedItem])
parseFromFilename s = do
  (fn, txt) <- getFileContentsStdin s
  case parse txt of
    Ex.Exception err ->
      handleParseError (X.unpack . L.unFilename $ fn) err
    Ex.Success g -> return (fn, g)

handleParseError
  :: String
  -- ^ Filename
  -> String
  -> IO a
handleParseError fn e = do
  pn <- getProgName
  IO.hPutStrLn IO.stderr $ pn
        ++ ": error: could not parse " ++ fn ++ ":"
  IO.hPutStrLn IO.stderr e
  Exit.exitFailure
