module Penny.Copper.TopLine where

import Control.Applicative ((<$>), (<*>), (<*), (<|>),
                            liftA)
import Text.Parsec ( optionMaybe, getParserState,
                     sourceLine, statePos, optionMaybe )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Meta as Meta
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Memos.Transaction as M
import qualified Penny.Copper.Flag as F
import qualified Penny.Copper.Number as N
import qualified Penny.Copper.Payees as P
import Penny.Copper.Util (lexeme, eol)
import qualified Penny.Lincoln.Transaction.Unverified as U

topLine ::
  DT.DefaultTimeZone
  -> Parser (U.TopLine, Meta.TopLineLine, Meta.TopMemoLine)
topLine dtz =
  f
  <$> M.memo
  <*> liftA toLine getParserState
  <*> lexeme (DT.dateTime dtz)
  <*> optionMaybe (lexeme F.flag)
  <*> optionMaybe (lexeme N.number)
  <*> optionMaybe (P.quotedPayee <|> P.unquotedPayee)
  <*  eol
  where
    f (me, tml) lin dt fl nu pa = (tl, lin, tml) where
      tl = U.TopLine dt fl nu pa me
    toLine = Meta.TopLineLine . Meta.Line . sourceLine . statePos
