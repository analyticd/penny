module Penny.Brenner.BofA.Parser where

import Control.Applicative ((<$>), (<*), (<$), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (isUpper)
import Data.Monoid (mconcat, First(First, getFirst))
import qualified Data.Time as T
import qualified Text.Parsec as P
import Text.Parsec (char, string, many, many1, satisfy, manyTill,
                    (<?>), try)
import Text.Parsec.String (Parser)
import qualified Data.Tree as T
import Data.Tree (Tree(Node))

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

-- | Finds a node with the given node value. Returns the first one it
-- finds, if there is one.
findNode :: (Eq a) => a -> Tree a -> Maybe (Tree a)
findNode x t@(Node l cs)
  | x == l = Just t
  | otherwise = getFirst . mconcat . map (First . findNode x) $ cs


safeRead :: (Read r) => String -> Maybe r
safeRead s = case reads s of
  (i,""):[] -> Just i
  _ -> Nothing

-- | Parses a B of A date-time. The format is YYYYMMDDHHMMSS. Discards
-- the HHMMSS.
parseDate :: String -> ExS T.Day
parseDate s =
  let (yr, r1) = splitAt 4 s
      (mo, r2) = splitAt 2 r1
      (da, _) = splitAt 2 r2
  in Ex.fromMaybe ("could not parse date: " ++ s) $ do
      yi <- safeRead yr
      ym <- safeRead mo
      yd <- safeRead da
      T.fromGregorianValid yi ym yd
