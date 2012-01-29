module Penny.Parser.Memos.Posting where

import Control.Monad ( liftM, void, when )
import Data.Text ( pack )
import Text.Parsec (
  Column, try, many1, char, getParserState, 
  sourceColumn, statePos, noneOf, manyTill, many )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits as B
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

data PostingFirstColumn = PostingFirstColumn Column
                          deriving Show

postingMemo ::
  PostingFirstColumn
  -> Parser B.Memo
postingMemo col = do
  (c:cs) <- liftM concat (many1 (try (postingMemoLine col)))
  return . B.Memo $ TextNonEmpty c (pack cs)

postingMemoLine ::
  PostingFirstColumn
  -- ^ Column that the posting line started at
  -> Parser String
postingMemoLine (PostingFirstColumn aboveCol) = do
  void (many (char ' '))
  st <- getParserState
  let currCol = sourceColumn . statePos $ st
  when (currCol <= aboveCol) $
    fail $ "memo line is not indented farther than corresponding "
    ++ "posting line"
  c <- noneOf "\t\n"
  cs <- manyTill (noneOf "\t\n") (char '\n')
  return (c : (cs ++ "\n"))
