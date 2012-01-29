module Penny.Parser where

import Text.Parsec (
  try, (<|>), anyChar, string, manyTill, satisfy,
  char, notFollowedBy, many, skipMany, optional,
  option, digit, choice,
  optionMaybe, many1, Column, sourceColumn, sourceLine,
  getParserState, noneOf, statePos, Line, eof,
  parse )
import Text.Parsec.Text ( Parser )

import Data.Char ( isLetter, isNumber, isPunctuation, isSymbol )
import qualified Data.Char as Char
import Control.Monad ( void, liftM, replicateM, when )
import Data.Text ( pack )
import Data.Time.LocalTime ( TimeZone )
import Control.Applicative ((<*>), pure)
import Data.Time (
  minutesToTimeZone, TimeOfDay, makeTimeOfDayValid,
  localTimeToUTC, midnight, LocalTime ( LocalTime ),
  Day, fromGregorianValid, getCurrentTimeZone )
                             
import Data.Fixed ( Pico )
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as F
import Data.Maybe ( catMaybes, isNothing )
import Text.Show.Pretty (ppShow)

import qualified Penny.Bits as B
import qualified Penny.Bits.DateTime as DT
import qualified Penny.Bits.Entry as E
import qualified Penny.Bits.Commodity as C
import qualified Penny.Bits.Amount as A
import qualified Penny.Bits.Price as Pr
import qualified Penny.Bits.PricePoint as PP
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )
import Penny.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ) )
import Penny.Groups.AtLeast2 ( AtLeast2 ( AtLeast2 ) )
import Penny.Bits.Qty ( Qty, partialNewQty )
import qualified Penny.Reports as R
import qualified Penny.Posting.Unverified.Parent as UPa
import qualified Penny.Posting.Unverified.Posting as UPo
import qualified Penny.Posting as P

newtype Radix = Radix { unRadix :: Char }
newtype Separator = Separator { unSeparator :: Char }
newtype DefaultTimeZone =
  DefaultTimeZone { unDefaultTimeZone :: TimeZone }

multiline :: Parser ()
multiline = let
  inner = try multiline <|> void anyChar
  in string "{-"
     >> manyTill inner (try (string "-}\n"))
     >> return ()

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

tagChar :: Parser Char
tagChar = satisfy (\l -> isLetter l || isNumber l)

tag :: Parser B.TagName
tag = do
  _ <- char '#'
  f <- tagChar
  r <- liftM pack (many tagChar)
  return . B.TagName $ TextNonEmpty f r

firstTag :: Parser B.TagName
firstTag = tag

-- Do not use Parsec's "space" parser for spaces. It parses any
-- whitespace character, including tabs and newlines. Instead use char
-- ' '.

-- | Tags must be separated by at least one space. (Is this a good
-- restriction? Does not seem to be necessary.)
nextTag :: Parser B.TagName
nextTag =
  char ' '
  >> skipMany (char ' ')
  >> tag

tags :: Parser B.Tags
tags = do
  f <- firstTag
  rs <- many (try nextTag)
  return $ B.Tags (f : rs)

drCr :: Parser E.DrCr
drCr = let
  dr = do
    void (char 'D')
    void (char 'r') <|> void (string "ebit")
    return E.Debit
  cr = do
    void (string "Cr")
    void (optional (string "edit"))
    return E.Credit
  in dr <|> cr

commoditySymbol :: Parser C.Commodity
commoditySymbol = do
  let p ch = Char.generalCategory ch == Char.CurrencySymbol
  c <- satisfy p
  return $ C.charCommodity c

subCommodity :: Parser C.SubCommodity
subCommodity = do
  c <- satisfy isLetter
  rs <- many $ satisfy (\l -> isLetter l || isNumber l)
  return (C.SubCommodity (TextNonEmpty c (pack rs)))

commodityLong :: Parser C.Commodity
commodityLong = do
  f <- subCommodity
  rs <- many $ do
    _ <- char ':'
    subCommodity
  return (C.Commodity (AtLeast1 f rs))

transactionPayee :: Parser B.Payee
transactionPayee = do
  c <- anyChar
  rs <- liftM pack (manyTill (noneOf "\n") (char '\n'))
  return . B.Payee $ TextNonEmpty c rs

qtyDigit :: Separator -> Parser Char
qtyDigit (Separator separator) = digit <|> (char separator >> digit)

radix :: Radix -> Parser Char
radix (Radix r) = char r >> return '.'

qty :: Radix -> Separator -> Parser Qty
qty rdx sep = let
  digitRun = do
    c <- digit
    cs <- many (qtyDigit sep)
    return (c : cs)
  withPoint = do
    l <- digitRun
    p <- radix rdx
    r <- digitRun
    return (l ++ (p : r))
  withoutPoint = digitRun
  in do
    s <- try withPoint <|> withoutPoint
    let d = read s
    return $ partialNewQty d

commoditySpaceQty ::
  Radix
  -> Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
commoditySpaceQty rdx sep = do
  c <- commoditySymbol <|> commodityLong
  void $ char ' '
  q <- qty rdx sep
  let fmt = R.CommodityFmt R.CommodityOnLeft R.SpaceBetween
  return (A.Amount q c, (c, fmt))

commodityQty ::
  Radix
  -> Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
commodityQty rdx sep = do
  c <- commoditySymbol
  q <- qty rdx sep
  let fmt = R.CommodityFmt R.CommodityOnLeft R.NoSpaceBetween
  return (A.Amount q c, (c, fmt))

qtyCommodity ::
  Radix
  -> Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
qtyCommodity rdx sep = do
  q <- qty rdx sep
  c <- commoditySymbol
  let fmt = R.CommodityFmt R.CommodityOnRight R.NoSpaceBetween
  return (A.Amount q c, (c, fmt))

qtySpaceCommodity ::
  Radix
  -> Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
qtySpaceCommodity rdx sep = do
  q <- qty rdx sep
  void $ char ' '
  c <- commoditySymbol <|> commodityLong
  let fmt = R.CommodityFmt R.CommodityOnRight R.SpaceBetween
  return (A.Amount q c, (c, fmt))

amount ::
  Radix
  -> Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
amount rdx sep =
  choice
  . map try
  $ [commoditySpaceQty, commodityQty,
     qtyCommodity, qtySpaceCommodity]
  <*> pure rdx
  <*> pure sep

{-
price ::
  Radix
  -> Separator
  -> Parser (Pr.To, Pr.CountPerUnit, R.CommodityFmt)
price rdx sep = do
  _ <- char '@'
  optional $ char ' '
  (am, (_, cf)) <- amount rdx sep
  let to = Pr.To $ A.commodity am
      cpu = Pr.CountPerUnit $ A.qty am
  return (to, cpu, cf)
-}

price ::
  DefaultTimeZone
  -> Radix
  -> Separator
  -> Parser (PP.PricePoint, (C.Commodity, R.CommodityFmt))
price dtz rad sep = do
  void $ char 'P'
  whitespace
  dt <- dateTime dtz
  whitespace
  com <- commoditySymbol <|> commodityLong
  whitespace
  (amt, pair) <- amount rad sep
  let (from, to) = (Pr.From com, Pr.To (A.commodity amt))
      cpu = Pr.CountPerUnit (A.qty amt)
  pr <- case Pr.price from to cpu of
    (Just pri) -> return pri
    Nothing -> fail "invalid price given"
  return (PP.PricePoint dt pr, pair)
  

-- Format for dates is:
-- 2011/01/22 or 2011-01-22
-- followed by a time spec:
-- 16:42 -0400
-- or HMS:
-- 16:42:45 -0400
-- You can omit the time zone spec, in which case
-- the parser assumes the time is local to the timezone that is
-- passed in to the function (generally this will be the local time
-- the machine is in.)

digits1or2 :: Parser String
digits1or2 = do
  d1 <- digit
  d2 <- optionMaybe digit
  let r = case d2 of
        Nothing -> d1:[]
        (Just d) -> d1:d:[]
  return r

monthOrDayNum :: Parser Int
monthOrDayNum = do
  i <- digits1or2
  return $ read i

year :: Parser Integer
year = do
  i <- replicateM 4 digit
  return $ read i

day :: Parser Day
day = do
  let slash = void $ char '/' <|> char '-'
  y <- year
  slash
  m <- monthOrDayNum
  slash
  d <- monthOrDayNum
  case fromGregorianValid y m d of
    Nothing -> fail "invalid date"
    (Just da) -> return da
  
hoursMins :: Parser (Int, Int)
hoursMins = do
  h <- digits1or2
  void $ char ':'
  m <- replicateM 2 digit
  return (read h, read m)

secs :: Parser Pico
secs = do
  void $ char ':'
  s <- replicateM 2 digit
  let fi = fromIntegral :: Integer -> Pico
  return (fi . read $ s)

timeOfDay :: Parser TimeOfDay
timeOfDay = do
  (h, m) <- hoursMins
  let fi = fromIntegral :: Int -> Pico
  s <- option (fi 0) secs
  case makeTimeOfDayValid h m s of
    Nothing -> fail "invalid time of day"
    (Just tod) -> return tod

sign :: Parser (Int -> Int)
sign = let
  pos = char '+' >> return id
  neg = char '-' >> return negate
  in pos <|> neg

timeZone :: Parser TimeZone
timeZone = do
  s <- sign
  hh <- replicateM 2 digit
  mm <- replicateM 2 digit
  let hr = read hh
      mi = read mm
      mins = s (hr * 60 + mi)
      zone = minutesToTimeZone mins
  return zone

dateTime ::
  DefaultTimeZone
  -> Parser DT.DateTime
dateTime (DefaultTimeZone dtz) = do
  d <- day
  maybeTime <- optionMaybe (try (char ' ' >> timeOfDay))
  (tod, tz) <- case maybeTime of
    Nothing -> return (midnight, dtz)
    (Just t) -> do
      maybeTz <- optionMaybe (try (char ' ' >> timeZone))
      case maybeTz of
        (Just zone) -> return (t, zone)
        Nothing -> return (t, dtz)
  let local = LocalTime d tod
      utc = localTimeToUTC tz local
  return $ DT.DateTime utc

number :: Parser B.Number
number = do
  void $ char '('
  let p l =  isLetter l || isNumber l
  c <- satisfy p
  cs <- manyTill (satisfy p) (char ')')
  return . B.Number $ TextNonEmpty c (pack cs)

postingPayee :: Parser B.Payee
postingPayee = do
  void $ char '<'
  let p c = notElem c "<>" && (isLetter c || isNumber c
            || isPunctuation c || isSymbol c || c == ' ')
  c <- satisfy p
  cs <- manyTill (satisfy p) (char '>')
  return . B.Payee $ TextNonEmpty c (pack cs)

oneLineComment :: Parser ()  
oneLineComment = do
  void $ try (string "//")
  void $ manyTill (satisfy (/= '\n')) (char '\n')

transactionMemoLine :: Parser String
transactionMemoLine = do
  void $ char ';'
  cs <- many1 (satisfy (/= '\n'))
  void $ char '\n'
  return (cs ++ "\n")

transactionMemo :: Parser B.Memo
transactionMemo = do
  (c:cs) <- liftM concat $ many1 transactionMemoLine
  return . B.Memo $ TextNonEmpty c (pack cs)

postingMemoLine ::
  PostingFirstColumn
  -- ^ Column that the posting line started at
  -> Parser String
postingMemoLine (PostingFirstColumn aboveCol) = do
  whitespace
  st <- getParserState
  let currCol = sourceColumn . statePos $ st
  when (currCol <= aboveCol) $
    fail $ "memo line is not indented farther than corresponding "
    ++ "posting line"
  c <- noneOf "\t\n"
  cs <- manyTill (noneOf "\t\n") (char '\n')
  return (c : (cs ++ "\n"))

postingMemo ::
  PostingFirstColumn
  -> Parser B.Memo
postingMemo col = do
  (c:cs) <- liftM concat (many1 (postingMemoLine col))
  return . B.Memo $ TextNonEmpty c (pack cs)

entry ::
  Radix
  -> Separator
  -> Parser (E.Entry, (C.Commodity, R.CommodityFmt))
entry rad sep = do
  dc <- drCr
  void $ many1 (char ' ')
  (am, p) <- amount rad sep
  let e = E.Entry dc am
  return (e, p)

flag :: Parser B.Flag
flag = do
  void $ char '['
  c <- anyChar
  void $ char ']'
  return $ B.Flag c

whitespace :: Parser ()
whitespace = void (many (char ' '))


parent :: DefaultTimeZone -> Parser UPa.Parent
parent dtz = do
  m <- optionMaybe transactionMemo
  d <- dateTime dtz
  whitespace
  f <- optionMaybe flag
  whitespace
  n <- optionMaybe number
  whitespace
  p <- optionMaybe transactionPayee
  when (isNothing p) (void $ char '\n')
  return $ UPa.Parent d f n p m

data PostingFirstColumn = PostingFirstColumn Column
                          deriving Show
data PostingLine = PostingLine Line
                   deriving Show

data PostingData =
  PostingData { firstColumn :: PostingFirstColumn
              , line :: PostingLine
              , unverified :: UPo.Posting
              , commodity :: Maybe C.Commodity
              , format :: Maybe R.CommodityFmt }
  deriving Show

posting :: Radix -> Separator -> Parser PostingData
posting rad sep = do
  void $ char ' '
  whitespace
  st <- getParserState
  let col = PostingFirstColumn . sourceColumn . statePos $ st
      lin = PostingLine . sourceLine . statePos $ st
  f <- optionMaybe flag
  whitespace
  n <- optionMaybe number
  whitespace
  p <- optionMaybe postingPayee
  whitespace
  a <- account
  whitespace
  t <- option (B.Tags []) tags
  whitespace
  (e, c, fmt) <- do
    me <- optionMaybe $ entry rad sep
    case me of
      (Just (e', (c', fmt'))) -> return (Just e', Just c', Just fmt')
      Nothing -> return (Nothing, Nothing, Nothing)
  whitespace
  void $ char '\n'
  m <- optionMaybe $ try (postingMemo col)
  let pd = PostingData col lin unv c fmt
      unv = UPo.Posting p n f a e t m
  return pd

data TransactionData =
  TransactionData { transaction :: P.Transaction
                  , postingLines :: AtLeast2 PostingLine
                  , formats :: [(C.Commodity, R.CommodityFmt)] }
  deriving Show

transactionParser ::
  DefaultTimeZone
  -> Radix
  -> Separator
  -> Parser TransactionData
transactionParser dtz rad sep = do
  pa <- parent dtz
  p1 <- posting rad sep
  p2 <- posting rad sep
  ps <- many (try (posting rad sep))
  let a2 = AtLeast2 p1 p2 ps
      errXact = P.transaction pa (fmap unverified a2)
  xact <- case errXact of
    (Ex.Exception err) -> fail $ errorStr err
    (Ex.Success x) -> return x
  let lns = fmap line a2
      fmtPairs = zip cs fs
      cs = catMaybes . F.toList . fmap commodity $ a2
      fs = catMaybes . F.toList . fmap format $ a2
  return $ TransactionData xact lns fmtPairs

errorStr :: P.Error -> String
errorStr e = case e of
  P.UnbalancedError -> "postings are not balanced"
  P.TooManyInferError -> "too many postings with entry amounts to infer"
  P.CouldNotInferError -> "could not infer entry for posting"

ledger ::
  DefaultTimeZone
  -> Radix
  -> Separator
  -> Parser [TransactionData]
ledger dtz rad sep = do
  let ignores = void (many (multiline <|> oneLineComment
                <|> void (char '\n')))
      t = do
        trans <- transactionParser dtz rad sep
        ignores
        return trans
  ignores
  manyTill t eof

      
------------------------------------------------------------
------------------------------------------------------------
-- test basement - prefix functions with _

_testParse :: FilePath -> IO ()
_testParse fp = do
  tz <- getCurrentTimeZone
  let rad = Radix '.'
      sep = Separator ','
  s <- readFile fp
  let o = parse (ledger (DefaultTimeZone tz) rad sep)
          fp (pack s)
  putStrLn (ppShow o)
