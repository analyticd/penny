module Penny.Brenner.Amex.Parsec where

import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), pure,
                            (<|>), optional)
import qualified Data.Time as Time
import qualified Penny.Brenner.Amex.Types as Y
import Text.Parsec.Text (Parser)
import qualified Text.Parsec as P
import Text.Parsec (many1, char, many, satisfy)
import qualified Penny.Lincoln as L
import qualified Penny.Copper.Parsec as CP
import qualified Data.Text as X
import qualified Data.Text.IO as TIO

-- | Loads incoming Amex transactions.
loadIncoming :: Y.CSVLocation -> IO [Y.Posting]
loadIncoming (Y.CSVLocation loc) = do
  txt <- TIO.readFile loc
  let parsed = P.parse (P.many posting <* P.eof) "" txt
  either (error . show) return parsed

skipThrough :: Char -> Parser ()
skipThrough c = () <$ many (satisfy (/= c)) <* char c

readThrough :: Char -> Parser String
readThrough c = many (satisfy (/= c)) <* char c

date :: Parser Y.Date
date = p >>= failOnErr
  where
    p = (,,)
        <$> fmap read (many1 P.digit)
        <*  char '/'
        <*> fmap read (many1 P.digit)
        <* char '/'
        <*> fmap read (many1 P.digit)
        <*  skipThrough ','
    failOnErr (m, d, y) = maybe (fail "could not parse date")
      (return . Y.Date)
      $ Time.fromGregorianValid y m d

incDecAmount :: Parser (Y.IncDec, Y.Amount)
incDecAmount =
  (,)
  <$> ((Y.Decrease <$ char '-') <|> pure Y.Increase)
  <*> (fmap Y.Amount $ readThrough ',')

doubleQuoted :: Parser String
doubleQuoted = char '"' *> readThrough '"'

desc :: Parser Y.Desc
desc = fmap Y.Desc $ doubleQuoted <* char ','

payee :: Parser Y.Payee
payee = fmap Y.Payee $ doubleQuoted <* char ','

amexId :: Parser Y.AmexId
amexId = fmap Y.AmexId
         $ char '"' *> char '\'' *> readThrough '\''
           <* char '"' <* char ','


-- | Skips a field. Will skip a quoted field or,
-- alternatively, skip everything through to the next comma. Do not
-- use for the last field, as it looks for a trailing comma.
skipField :: Parser ()
skipField =
  ()
  <$ skipper
  <* char ','
  where
    skipper =     (() <$ optional doubleQuoted)
              <|> (() <$ many (satisfy (/= ',')))


-- | Parses last field (currently unknown). Parsers the EOL character.
skipLast :: Parser ()
skipLast = skipThrough '\n'

posting :: Parser Y.Posting
posting =
  f
  <$> date                                        -- 1
  <*  skipField                                   -- 2 Unknown
  <*> desc                                        -- 3 Description
  <*  skipField                                   -- 4 Unknown
  <*  skipField                                   -- 5 Unknown
  <*  skipField                                   -- 6 Unknown
  <*  skipField                                   -- 7 Unknown
  <*> incDecAmount                                -- 8
  <*  skipField                                   -- 9 Unknown
  <*  skipField                                   -- 10 Category
  <*> payee                                       -- 11 D.B.A.
  <*  skipField                                   -- 12 Address
  <*  skipField                                   -- 13 Postcode
  <*> amexId                                      -- 14
  <*  skipField                                   -- 15 Unknown
  <*  skipLast                                    -- 16
  where
    f dt ds (t, a) p i = Y.Posting dt ds t a p i

parseEntry :: Y.Currency -> Y.Posting -> L.Entry
parseEntry (Y.Currency cy) a = L.Entry dc am
  where
    dc = case Y.incDec a of
      Y.Increase -> L.Credit
      Y.Decrease -> L.Debit
    am = L.Amount q cy
         (Just L.CommodityOnLeft) (Just L.NoSpaceBetween)
    parser = CP.quantity <* P.eof
    txt = X.pack . Y.unAmount . Y.amount $ a
    q = case P.parse parser "" txt of
      Left e -> error $ "could not parse quantity " ++ show txt
                ++ ": " ++ show e
      Right g -> g

