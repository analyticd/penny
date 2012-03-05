module Penny.Copper.TopLine where

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), (<$),
                            (<**>), pure)
import Control.Monad ( void, when, liftM )
import Data.Maybe (isNothing)
import Text.Parsec ( optionMaybe, many, char, getParserState,
                     sourceLine, statePos, optionMaybe )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Meta as Meta
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Memos.Transaction as M
import qualified Penny.Copper.Flag as F
import qualified Penny.Copper.Number as N
import qualified Penny.Copper.Payees.Transaction as Payee
import qualified Penny.Lincoln.Transaction.Unverified as U

whitespace :: Parser ()
whitespace = void (many (char ' '))

topLine :: DT.DefaultTimeZone
           -> Parser (U.TopLine, Meta.TopLineLine,
                      (Maybe Meta.TopMemoLine))
topLine dtz = do
  m <- optionMaybe M.memo
  let (mem, tml) = case m of
        (Just (me, tm)) -> (Just me, Just tm)
        Nothing -> (Nothing, Nothing)
  line <- liftM ( Meta.TopLineLine
                 . Meta.Line
                 . sourceLine
                 . statePos) getParserState
  d <- DT.dateTime dtz
  whitespace
  f <- optionMaybe F.flag
  whitespace
  n <- optionMaybe N.number
  whitespace
  p <- optionMaybe Payee.payee
  when (isNothing p) (void $ char '\n')
  return (U.TopLine d f n p mem, line, tml)

