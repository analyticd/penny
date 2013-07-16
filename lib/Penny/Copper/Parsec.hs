-- | Parsec parsers for the ledger file format. The format is
-- documented in EBNF in the file @doc\/ledger-grammar.org@.
module Penny.Copper.Parsec where

import qualified Penny.Copper.Interface as I
import qualified Penny.Copper.Terminals as T
import Text.Parsec.Text (Parser)
import Text.Parsec (many, many1, satisfy)
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as Pos
import Control.Applicative.Permutation (runPerms, maybeAtom)
import Control.Applicative ((<$>), (<$), (<*>), (*>), (<*),
                            (<|>), optional)
import Control.Monad (replicateM, when)
import qualified Control.Monad.Exception.Synchronous as Ex
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
    p = (L.RadFrac <$> (satisfy T.period *> digitSequence))
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

leftCmdtyLvl1Amt :: Parser (L.Amount, L.Side, L.SpaceBetween)
leftCmdtyLvl1Amt =
  f <$> quotedLvl1Cmdty <*> spaceBetween <*> quantity
  where
    f c s q = (L.Amount q c , L.CommodityOnLeft, s)

leftCmdtyLvl3Amt :: Parser (L.Amount, L.Side, L.SpaceBetween)
leftCmdtyLvl3Amt = f <$> lvl3Cmdty <*> spaceBetween <*> quantity
  where
    f c s q = (L.Amount q c, L.CommodityOnLeft, s)

leftSideCmdtyAmt :: Parser (L.Amount, L.Side, L.SpaceBetween)
leftSideCmdtyAmt = leftCmdtyLvl1Amt <|> leftCmdtyLvl3Amt

rightSideCmdty :: Parser L.Commodity
rightSideCmdty = quotedLvl1Cmdty <|> lvl2Cmdty

rightSideCmdtyAmt :: Parser (L.Amount, L.Side, L.SpaceBetween)
rightSideCmdtyAmt =
  f <$> quantity <*> spaceBetween <*> rightSideCmdty
  where
    f q s c = (L.Amount q c ,L.CommodityOnRight, s)


amount :: Parser (L.Amount, L.Side, L.SpaceBetween)
amount = leftSideCmdtyAmt <|> rightSideCmdtyAmt

comment :: Parser I.Comment
comment =
  (I.Comment . pack)
  <$ satisfy T.hash
  <*> many (satisfy T.nonNewline)
  <* satisfy T.newline
  <* many (satisfy T.white)

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

debit :: Parser L.DrCr
debit = L.Debit <$ satisfy T.lessThan

credit :: Parser L.DrCr
credit = L.Credit <$ satisfy T.greaterThan

drCr :: Parser L.DrCr
drCr = debit <|> credit

entry :: Parser (L.Entry, L.Side, L.SpaceBetween)
entry = f <$> drCr <* (many (satisfy T.white)) <*> amount
  where
    f dc (am, sd, sb) = (L.Entry dc am, sd, sb)

flag :: Parser L.Flag
flag = (L.Flag . pack) <$ satisfy T.openSquare
  <*> many (satisfy T.flagChar) <* satisfy (T.closeSquare)

postingMemoLine :: Parser Text
postingMemoLine =
  pack
  <$ satisfy T.apostrophe
  <*> many (satisfy T.nonNewline)
  <* satisfy T.newline <* many (satisfy T.white)

postingMemo :: Parser L.Memo
postingMemo = L.Memo <$> many1 postingMemoLine

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


number :: Parser L.Number
number =
  L.Number . pack <$ satisfy T.openParen
  <*> many (satisfy T.numberChar) <* satisfy T.closeParen

lvl1Payee :: Parser L.Payee
lvl1Payee = L.Payee . pack <$> many (satisfy T.quotedPayeeChar)

quotedLvl1Payee :: Parser L.Payee
quotedLvl1Payee = satisfy T.tilde *> lvl1Payee <* satisfy T.tilde

lvl2Payee :: Parser L.Payee
lvl2Payee = (\c cs -> L.Payee (pack (c:cs))) <$> satisfy T.letter
            <*> many (satisfy T.nonNewline)

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

tag :: Parser L.Tag
tag = L.Tag . pack <$ satisfy T.asterisk <*> many (satisfy T.tagChar)
      <* many (satisfy T.white)

tags :: Parser L.Tags
tags = (\t ts -> L.Tags (t:ts)) <$> tag <*> many tag

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

flagNumPayee :: Parser (Maybe L.Flag, Maybe L.Number, Maybe L.Payee)
flagNumPayee = runPerms
  ( (,,) <$> maybeAtom (flag <* skipWhite)
         <*> maybeAtom (number <* skipWhite)
         <*> maybeAtom (quotedLvl1Payee <* skipWhite) )

postingAcct :: Parser L.Account
postingAcct = quotedLvl1Acct <|> lvl2Acct

posting :: Parser (L.PostingCore, L.PostingLine, Maybe L.Entry)
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

transaction :: Parser I.ParsedTxn
transaction = do
  ptl <- topLine
  let getEntPair (core, lin, mayEn) = (mayEn, (core, lin))
  ts <- fmap (map getEntPair) $ many posting
  ents <- maybe (fail "unbalanced transaction") return $ L.ents ts
  return (ptl, ents)


blankLine :: Parser ()
blankLine = () <$ satisfy T.newline <* skipWhite

item :: Parser I.ParsedItem
item
  = fmap S.S4a transaction
  <|> fmap S.S4b price
  <|> fmap S.S4c comment
  <|> (S.S4d I.BlankLine) <$ blankLine


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
