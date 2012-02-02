module Penny.Copper.TopLine where

import Control.Monad ( void, when, liftM )
import Data.Maybe (isNothing)
import Text.Parsec ( optionMaybe, many, char, getParserState,
                     sourceLine, statePos )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Meta as Meta
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Memos.Transaction as M
import qualified Penny.Copper.Flag as F
import qualified Penny.Copper.Number as N
import qualified Penny.Copper.Payees.Transaction as Payee
import qualified Penny.Lincoln.Transaction.Unverified as U

whitespace :: Parser ()
whitespace = void (many (char ' '))

topLine :: DT.DefaultTimeZone -> Parser (U.TopLine, Meta.TopLineLine)
topLine dtz = do
  line <- liftM ( Meta.TopLineLine
                 . Meta.Line
                 . sourceLine
                 . statePos) getParserState
  m <- optionMaybe M.memo
  d <- DT.dateTime dtz
  whitespace
  f <- optionMaybe F.flag
  whitespace
  n <- optionMaybe N.number
  whitespace
  p <- optionMaybe Payee.payee
  when (isNothing p) (void $ char '\n')
  return (U.TopLine d f n p m, line)

