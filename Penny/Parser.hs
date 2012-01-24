module Penny.Parser where

import Text.Parsec (
  try, (<|>), anyChar, string, manyTill, satisfy,
  char, notFollowedBy, many, skipMany, optional,
  option, digit, getState, putState, choice,
  optionMaybe, lookAhead, many1, Column, sourceColumn,
  getParserState)
import Text.Parsec.Text ( Parser )

import Data.Char ( isLetter, isNumber, isPunctuation, isSymbol )
import qualified Data.Char as Char
import Control.Monad ( void, liftM, replicateM, when )
import Data.Text ( pack, empty, snoc )
import Data.Time.LocalTime ( TimeZone )
import qualified Data.Map as M
import Data.Map ( Map )
import Control.Applicative ((<*>), pure)
import Data.Time.Calendar ( Day, fromGregorianValid )
import Data.Time.LocalTime ( TimeZone, minutesToTimeZone,
                             TimeOfDay, makeTimeOfDayValid,
                             localTimeToUTC, midnight,
                             LocalTime ( LocalTime ) )
import Data.Time.Clock ( UTCTime )
import Data.Fixed ( Pico )

import qualified Penny.Posting as P
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )
import Penny.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ) )
import Penny.Qty ( Qty, partialNewQty )
import qualified Penny.Reports as R

newtype Radix = Radix { unRadix :: Char }
newtype Separator = Separator { unSeparator :: Char }
newtype DefaultTimeZone =
  DefaultTimeZone { unDefaultTimeZone :: TimeZone }

multiline :: Parser ()
multiline = let
  inner = try multiline <|> void anyChar
  in string "{-"
     >> manyTill inner (try (string "-}"))
     >> return ()

subAccountChar :: Parser Char
subAccountChar = let
  notSpc = satisfy (\l -> isLetter l || isNumber l)
  spc = do
    void $ char ' '
    notFollowedBy (char ' ' <|> char '#')
    return ' '
  in notSpc <|> try spc

subAccountName :: Parser P.SubAccountName
subAccountName = do
  c <- subAccountChar
  r <- liftM pack $ many subAccountChar
  return . P.SubAccountName $ TextNonEmpty c r

firstSubAccount :: Parser P.SubAccountName
firstSubAccount = subAccountName

nextSubAccount :: Parser P.SubAccountName
nextSubAccount = char ':' >> subAccountName

account :: Parser P.Account
account = do
  f <- firstSubAccount
  r <- many nextSubAccount
  return . P.Account $ AtLeast1 f r

tagChar :: Parser Char
tagChar = satisfy (\l -> isLetter l || isNumber l)

tag :: Parser P.TagName
tag = do
  _ <- char '#'
  f <- tagChar
  r <- liftM pack (many tagChar)
  return . P.TagName $ TextNonEmpty f r

firstTag :: Parser P.TagName
firstTag = tag

-- Do not use Parsec's "space" parser for spaces. It parses any
-- whitespace character, including tabs and newlines. Instead use char
-- ' '.

-- | Tags must be separated by at least one space. (Is this a good
-- restriction? Does not seem to be necessary.)
nextTag :: Parser P.TagName
nextTag =
  skipMany multiline
  >> char ' '
  >> skipMany (multiline <|> void (char ' '))
  >> tag

tags :: Parser P.Tags
tags = do
  f <- firstTag
  rs <- many (try nextTag)
  return $ P.Tags (f : rs)

drCr :: Parser P.DrCr
drCr = let
  dr = do
    void (char 'D')
    void (char 'r') <|> void (string "ebit")
    return P.Debit
  cr = do
    void (string "Cr")
    void (optional (string "edit"))
    return P.Credit
  in dr <|> cr

commoditySymbol :: Parser P.Commodity
commoditySymbol = do
  let p ch = Char.generalCategory ch == Char.CurrencySymbol
  c <- satisfy p
  return . P.Commodity $ TextNonEmpty c empty

commodityLong :: Parser P.Commodity
commodityLong = do
  c <- satisfy isLetter
  rs <- many $ satisfy (\l -> isLetter l || isNumber l)
  return . P.Commodity $ TextNonEmpty c (pack rs)

priceDesc :: Parser P.PriceDesc
priceDesc = do
  void $ char '@'
  option P.UnitPrice (char '@' >> return P.TotalPrice)

transactionPayeeChar :: Parser Char
transactionPayeeChar = let
  spc = do
    void $ char ' '
    notFollowedBy (void (char ' ') <|> (void (try (string "--")))
                   <|> (void (try (string "{-"))))
    return ' '
  p c = isLetter c || isNumber c
        || isPunctuation c || isSymbol c
  in satisfy p <|> try spc

transactionPayee :: Parser P.Payee
transactionPayee = do
  c <- transactionPayeeChar
  rs <- liftM pack (many transactionPayeeChar)
  return . P.Payee $ TextNonEmpty c rs

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
  -> Parser (P.Amount, (P.Commodity, R.CommodityFmt))
commoditySpaceQty rdx sep = do
  c <- commoditySymbol <|> commodityLong
  void $ char ' '
  q <- qty rdx sep
  let fmt = R.CommodityFmt R.CommodityOnLeft R.SpaceBetween
  return (P.Amount q c, (c, fmt))

commodityQty ::
  Radix
  -> Separator
  -> Parser (P.Amount, (P.Commodity, R.CommodityFmt))
commodityQty rdx sep = do
  c <- commoditySymbol
  q <- qty rdx sep
  let fmt = R.CommodityFmt R.CommodityOnLeft R.NoSpaceBetween
  return (P.Amount q c, (c, fmt))

qtyCommodity ::
  Radix
  -> Separator
  -> Parser (P.Amount, (P.Commodity, R.CommodityFmt))
qtyCommodity rdx sep = do
  q <- qty rdx sep
  c <- commoditySymbol
  let fmt = R.CommodityFmt R.CommodityOnRight R.NoSpaceBetween
  return (P.Amount q c, (c, fmt))

qtySpaceCommodity ::
  Radix
  -> Separator
  -> Parser (P.Amount, (P.Commodity, R.CommodityFmt))
qtySpaceCommodity rdx sep = do
  q <- qty rdx sep
  void $ char ' '
  c <- commoditySymbol <|> commodityLong
  let fmt = R.CommodityFmt R.CommodityOnRight R.SpaceBetween
  return (P.Amount q c, (c, fmt))

amount ::
  Radix
  -> Separator
  -> Parser (P.Amount, (P.Commodity, R.CommodityFmt))
amount rdx sep =
  choice
  . map try
  $ [commoditySpaceQty, commodityQty,
     qtyCommodity, qtySpaceCommodity]
  <*> pure rdx
  <*> pure sep

price ::
  Radix
  -> Separator
  -> Parser (P.Price, (P.Commodity, R.CommodityFmt))
price rdx sep = do
  pd <- priceDesc
  optional $ char ' '
  (am, cp) <- amount rdx sep
  let p = P.Price pd am
  return (p, cp)

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
  return (fromIntegral . read $ s)

timeOfDay :: Parser TimeOfDay
timeOfDay = do
  (h, m) <- hoursMins
  s <- option (fromIntegral 0) secs
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
  -> Parser P.DateTime
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
  return $ P.DateTime utc

cleared :: Parser P.Cleared
cleared = let
  clear = do
    void $ char '*'
    lookAhead $ char ' '
    return P.Cleared
  in option P.NotCleared (try clear)

number :: Parser P.Number
number = do
  void $ char '('
  let p l =  isLetter l || isNumber l
  c <- satisfy p
  cs <- manyTill (satisfy p) (char ')')
  return . P.Number $ TextNonEmpty c (pack cs)

postingPayee :: Parser P.Payee
postingPayee = do
  void $ char '<'
  let p c = notElem c "<>" && (isLetter c || isNumber c
            || isPunctuation c || isSymbol c || c == ' ')
  c <- satisfy p
  cs <- manyTill (satisfy p) (char '>')
  return . P.Payee $ TextNonEmpty c (pack cs)

oneLineComment :: Parser ()  
oneLineComment = do
  void $ try (string "--")
  void $ manyTill (satisfy (/= '\n')) (char '\n')

transactionMemoLine :: Parser String
transactionMemoLine = do
  void $ char ';'
  cs <- many1 (satisfy (/= '\n'))
  void $ char '\n'
  return (cs ++ "\n")

transactionMemo :: Parser P.TransactionMemo
transactionMemo = do
  (c:cs) <- liftM concat $ many1 transactionMemoLine
  return . P.TransactionMemo $ TextNonEmpty c (pack cs)

postingMemo ::
  Column
  -- ^ Column that the posting line started at

  -> Parser P.PostingMemo

postingMemo aboveCol = do
  st <- getParserState
  let currCol = sourceColumn st
  when (currCol <= aboveCol) $
    fail "memo line is not indented farther than corresponding "
    ++ "posting line"
  


