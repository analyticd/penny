module Penny.Parser.Parent where

import Control.Monad ( void, when )
import Data.Maybe (isNothing)
import Text.Parsec ( optionMaybe, many1, char )
import Text.Parsec.Text ( Parser )

import qualified Penny.Parser.DateTime as DT
import qualified Penny.Parser.Memos.Transaction as M
import qualified Penny.Parser.Flag as F
import qualified Penny.Parser.Number as N
import qualified Penny.Parser.Payees.Transaction as Payee
import qualified Penny.Posting.Unverified.Parent as Parent

whitespace :: Parser ()
whitespace = void (many1 (char ' '))

parent :: DT.DefaultTimeZone -> Parser Parent.Parent
parent dtz = do
  m <- optionMaybe M.memo
  d <- DT.dateTime dtz
  whitespace
  f <- optionMaybe F.flag
  whitespace
  n <- optionMaybe N.number
  whitespace
  p <- optionMaybe Payee.payee
  when (isNothing p) (void $ char '\n')
  return $ Parent.Parent d f n p m

