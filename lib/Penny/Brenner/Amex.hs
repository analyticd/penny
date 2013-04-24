-- | Parses Amex credit card data. See the help text in the 'help'
-- function for more details. Also, the file format is documented in
-- the file @doc\/amex-file-format.org@.
module Penny.Brenner.Amex (parser) where

import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), pure,
                            (<|>), optional)
import qualified Data.Time as Time
import qualified Penny.Brenner.Types as Y
import Text.Parsec.Text (Parser)
import qualified Text.Parsec as P
import Text.Parsec (many1, char, many, satisfy, (<?>))
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Control.Monad.Exception.Synchronous as Ex

parser :: (String, Y.FitFileLocation
                   -> IO (Ex.Exceptional String [Y.Posting]))
parser = (help, loadIncoming)

help :: String
help = unlines
  [ "Parses American Express transaction data in CSV format."
  , "Not tested with American Express deposit accounts."
  , "To download, click the \"Download\" link which is visible"
  , "above and to the right of the transaction list. Under"
  , "\"Download Current View\" Select \"CSV\""
  , "and be sure to click"
  , "\"Include all additional Transaction Details.\""
  ]

-- | Loads incoming Amex transactions.
loadIncoming :: Y.FitFileLocation
             -> IO (Ex.Exceptional String [Y.Posting])
loadIncoming (Y.FitFileLocation loc) = do
  txt <- TIO.readFile loc
  let parsed = P.parse (P.many posting <* P.eof) "" txt
      err s = "could not parse incoming postings: " ++ show s
  return (Ex.mapException err . Ex.fromEither $ parsed)

skipThrough :: Char -> Parser ()
skipThrough c = () <$ many (satisfy (/= c)) <* char c

readThrough :: Char -> Parser String
readThrough c = many (satisfy (/= c)) <* char c

date :: Parser Y.Date
date = p >>= failOnErr <?> "date"
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
incDecAmount = do
  { incDec <- (Y.Decrease <$ char '-') <|> pure Y.Increase
  ; amtStr <- readThrough ','
  ; case Y.mkAmount amtStr of
      Nothing -> fail $ "could not parse amount: " ++ amtStr
      Just a -> return (incDec, a)
  } <?> "increase or decrease amount"

doubleQuoted :: Parser String
doubleQuoted = char '"' *> readThrough '"' <?> "double quoted field"

desc :: Parser Y.Desc
desc = (Y.Desc . X.pack)
       <$> (doubleQuoted <* char ',')
       <?> "description"

payee :: Parser Y.Payee
payee = (Y.Payee . X.pack)
        <$> (doubleQuoted <* char ',')
        <?> "payee"

amexId :: Parser Y.FitId
amexId = (Y.FitId . X.pack)
         <$> ( char '"' *> char '\'' *> readThrough '\''
               <* char '"' <* char ','
             )
         <?> "Amex ID"


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
skipLast = skipThrough '\n' <?> "last field"

posting :: Parser Y.Posting
posting =
  f
  <$> date                                        -- 1
  <*  (skipField <?> "field 2")                   -- 2 Unknown
  <*> desc                                        -- 3 Description
  <*  (skipField <?> "field 4")                   -- 4 Unknown
  <*  (skipField <?> "field 5")                   -- 5 Unknown
  <*  (skipField <?> "field 6")                   -- 6 Unknown
  <*  (skipField <?> "field 7")                   -- 7 Unknown
  <*> incDecAmount                                -- 8
  <*  (skipField <?> "field 9")                   -- 9 Unknown
  <*  (skipField <?> "field 10")                  -- 10 Category
  <*> payee                                       -- 11 D.B.A.
  <*  (skipField <?> "field 12 - address")        -- 12 Address
  <*  (skipField <?> "field 13 - postcode")       -- 13 Postcode
  <*> amexId                                      -- 14
  <*  (skipField <?> "field 15")                  -- 15 Unknown
  <*  skipLast                                    -- 16
  where
    f dt ds (t, a) p i = Y.Posting dt ds t a p i
