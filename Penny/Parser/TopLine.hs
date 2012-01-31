module Penny.Parser.TopLine where

import Control.Monad ( void, when, liftM )
import Data.Maybe (isNothing)
import Text.Parsec ( optionMaybe, many, char, getParserState,
                     sourceLine, statePos )
import Text.Parsec.Text ( Parser )

import qualified Penny.Parser.DateTime as DT
import qualified Penny.Parser.Memos.Transaction as M
import qualified Penny.Parser.Flag as F
import qualified Penny.Parser.Number as N
import qualified Penny.Parser.Payees.Transaction as Payee
import qualified Penny.Posting.Unverified.TopLine as TopLine
import qualified Penny.Posting.Meta.TopLine as Meta

whitespace :: Parser ()
whitespace = void (many (char ' '))

topLine :: DT.DefaultTimeZone -> Parser (TopLine.TopLine, Meta.Line)
topLine dtz = do
  line <- liftM (Meta.Line . sourceLine . statePos) getParserState
  m <- optionMaybe M.memo
  d <- DT.dateTime dtz
  whitespace
  f <- optionMaybe F.flag
  whitespace
  n <- optionMaybe N.number
  whitespace
  p <- optionMaybe Payee.payee
  when (isNothing p) (void $ char '\n')
  return (TopLine.TopLine d f n p m, line)

