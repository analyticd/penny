module Penny.Copper.TopLine (
  topLine
  , render
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (<|>), pure)
import qualified Data.Text as X
import Text.Parsec ( optionMaybe, getPosition,
                     sourceLine, optionMaybe )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Memos.Transaction as M
import qualified Penny.Copper.Flag as F
import qualified Penny.Copper.Number as N
import qualified Penny.Copper.Payees as P
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Transaction as T
import Penny.Copper.Util (lexeme, eol, renMaybe, txtWords)
import qualified Penny.Lincoln.Transaction.Unverified as U

topLine :: Parser U.TopLine
topLine =
  f
  <$> M.memo
  <*> getPosition
  <*> lexeme DT.dateTime
  <*> optionMaybe (lexeme F.flag)
  <*> optionMaybe (lexeme N.number)
  <*> optionMaybe (P.quotedPayee <|> P.unquotedPayee)
  <*  eol
  where
    f (me, tml) pos dt fl nu pa = tl where
      tl = U.TopLine dt fl nu pa me meta
      meta = L.emptyTopLineMeta { L.topMemoLine = Just tml
                                , L.topLineLine = Just tll }
      tll = L.TopLineLine . sourceLine $ pos


