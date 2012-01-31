module Penny.Parser.Memos.Posting where

import Control.Monad ( liftM, void, when )
import Data.Text ( pack )
import Text.Parsec (
  try, many1, char, getParserState, 
  sourceColumn, statePos, noneOf, manyTill, many )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits as B
import qualified Penny.Meta.Posting as M
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

memo :: M.Column
        -> Parser B.Memo
memo col = do
  (c:cs) <- liftM concat (many1 (try (postingMemoLine col)))
  return . B.Memo $ TextNonEmpty c (pack cs)

postingMemoLine ::
  M.Column
  -- ^ Column that the posting line started at
  -> Parser String
postingMemoLine (M.Column aboveCol) = do
  void (many (char ' '))
  st <- getParserState
  let currCol = sourceColumn . statePos $ st
  when (currCol <= aboveCol) $
    fail $ "memo line is not indented farther than corresponding "
    ++ "posting line"
  c <- noneOf "\t\n"
  cs <- manyTill (noneOf "\t\n") (char '\n')
  return (c : (cs ++ "\n"))
