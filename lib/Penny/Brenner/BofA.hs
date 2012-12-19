-- | Parses statements for Bank of America deposit accounts. See the
-- help text in the 'help' function for more details. Also, the file
-- format is documented in the file @doc\/bofa-file-format.org@.
module Penny.Brenner.BofA (parser) where

import Control.Applicative ((<$>), (<*), (<$), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (isUpper)
import qualified Data.Time as T
import qualified Text.Parsec as P
import Text.Parsec (char, string, many, many1, satisfy, manyTill,
                    (<?>), try)
import Text.Parsec.String (Parser)
import qualified Data.Tree as T
import Data.Tree (Tree(Node))
import qualified Penny.Brenner.Types as Y
import qualified Data.Text as X

newtype TagName = TagName { unTagName :: String }
  deriving (Eq, Show)

newtype TagData = TagData { unTagData :: String }
  deriving (Eq, Show)

data Label
  = Parent TagName
  | Terminal TagName TagData
  deriving (Eq, Show)

type ExS = Ex.Exceptional String

bOfAFile :: Parser ([(TagName, TagData)], Tree Label)
bOfAFile =
  (,)
  <$> many headerLine
  <*  string "\r\n"
  <*> node

notReturn :: Parser Char
notReturn = satisfy (/= '\r')

headerLine :: Parser (TagName, TagData)
headerLine =
  (,)
  <$> (TagName <$> manyTill (satisfy isUpper) (char ':'))
  <*> (TagData <$> manyTill notReturn (char '\r')
               <*  char '\n')

openTag :: Parser String
openTag = do
  { let pc = (satisfy (\c -> c /= '/' && c /= '>'))
  ; c <- try (char '<' >> pc)
  ; cs <- many pc
  ; _ <- char '>'
  ; return (c:cs)
  } <?> "open tag"

closeTag :: String -> Parser ()
closeTag s = () <$ string "</" <* string s <* char '>'
             <?> "close tag named " ++ s

-- | Reads in a tag, then examine what's next. If a backslash-r is
-- next, then this is the end of the line. That means it a nested
-- tag. Parse some more child nodes, then parse a closing node. If
-- anything else is next, this is a data node. Parse the data, then
-- return that node.
node :: Parser (Tree Label)
node = do
  tagName <- openTag
  next <- P.anyChar
  case next of
    '\r' -> do
      _ <- char '\n'
      kids <- many1 node
      closeTag tagName
      _ <- string "\r\n"
      return $ T.Node (Parent (TagName tagName)) kids
    o -> do
      rs <- manyTill notReturn (char '\r')
      _ <- char '\n'
      return $
        T.Node (Terminal (TagName tagName) (TagData $ o:rs)) []

findNodes :: Eq a => a -> Tree a -> [Tree a]
findNodes x t@(Node l cs)
  | x == l = [t]
  | otherwise = concatMap (findNodes x) cs

safeRead :: (Read r) => String -> Maybe r
safeRead s = case reads s of
  (i,""):[] -> Just i
  _ -> Nothing

-- | Parses a B of A date-time. The format is YYYYMMDDHHMMSS. Discards
-- the HHMMSS.
parseDateStr :: String -> ExS Y.Date
parseDateStr s =
  let (yr, r1) = splitAt 4 s
      (mo, r2) = splitAt 2 r1
      (da, _) = splitAt 2 r2
  in Ex.fromMaybe ("could not parse date: " ++ s) $ do
      yi <- safeRead yr
      ym <- safeRead mo
      yd <- safeRead da
      Y.Date <$> T.fromGregorianValid yi ym yd

parseAmountStr :: String -> ExS (Y.IncDec, Y.Amount)
parseAmountStr s = do
  (f, rs) <- case s of
    "" -> Ex.throw "empty string for amount"
    x:xs -> return (x, xs)
  let (amtStr, incDec) = case f of
        '-' -> (rs, Y.Decrease)
        _ -> (s, Y.Increase)
  amt <- Ex.fromMaybe ("could not parse amount: " ++ s)
         $ Y.mkAmount amtStr
  return (incDec, amt)

postings :: Tree Label -> ExS [Y.Posting]
postings t =
  let match = Parent (TagName "STMTTRN")
  in mapM posting .findNodes match $ t

posting :: Tree Label -> ExS Y.Posting
posting (Node l cs) = do
  tag <- case l of
    Parent n -> return n
    _ -> Ex.throw "did not find posting tree"
  Ex.assert "did not find STMTTRN tag" $ unTagName tag == "STMTTRN"
  (tPosted, tAmt, tId, tName) <- case cs of
    _:t2:t3:t4:t5:[] -> return (t2, t3, t4, t5)
    _ -> Ex.throw "did not find five child nodes"
  pPosted <- parsePosted tPosted
  (amtIncDec, pAmt) <- parseAmount tAmt
  pId <- parseId tId
  pName <- parseName tName
  let pPayee = Y.Payee (X.empty)
  return $ Y.Posting pPosted pName amtIncDec pAmt pPayee pId

-- | Removes the TagData from a tree, after ensuring that the TagName
-- is correct and that the tree has no children.
terminalData
  :: String
  -- ^ The name of the terminal

  -> Tree Label

  -> ExS X.Text
  -- ^ Returns the data from the tag, or an error if this is not a
  -- terminal or if the terminal has children.
terminalData n (Node l cs) = do
  (tn, td) <- case l of
    Parent _ -> Ex.throw $ "looking for data tag named " ++ n
                           ++ ", but that tag does not have data"
    Terminal x y -> return (x, y)
  let tagErr = "looking for tag named " ++ n
        ++ ", but found tag named " ++ unTagName tn
  Ex.assert tagErr $ tn == TagName n
  let kidsErr = "data tag " ++ n ++ " should have no children,"
                ++ " but does"
  Ex.assert kidsErr $ null cs
  return . X.pack . unTagData $ td

parsePosted :: Tree Label -> ExS Y.Date
parsePosted t = do
  d <- terminalData "DTPOSTED" t
  parseDateStr (X.unpack d)

parseAmount :: Tree Label -> ExS (Y.IncDec, Y.Amount)
parseAmount t = do
  d <- terminalData "TRNAMT" t
  parseAmountStr (X.unpack d)

parseId :: Tree Label -> ExS Y.FitId
parseId t = do
  d <- terminalData "FITID" t
  return . Y.FitId $ d

parseName :: Tree Label -> ExS Y.Desc
parseName t = do
  d <- terminalData "NAME" t
  return . Y.Desc $ d

help :: String
help = unlines
  [ "Parses Bank of America postings for deposit accounts, like checking"
  , "or savings. This parser is not tested with credit card accounts."
  , "To download the data, from the account activity screen click on"
  , "\"Download\", which is just above all the transaction information."
  , "Then download the \"WEB Connect for Quicken 2010 and above.\""
  ]

parser :: (String, Y.FitFileLocation
                   -> IO (Ex.Exceptional String [Y.Posting]))
parser = (help, psr)
  where
    psr (Y.FitFileLocation path) = do
      str <- readFile path
      return $ case P.parse bOfAFile "" str of
        Left e -> Ex.throw
                  $ "could not parse Bank of America transactions: "
                    ++ show e
        Right (_, t) -> postings t
