module Penny.Copper.TopLine (
  topLine
  , render
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (<|>),
                            liftA, pure)
import qualified Data.Text as X
import Text.Parsec ( optionMaybe, getPosition,
                     sourceLine, statePos, optionMaybe, 
                     sourceName )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Meta as Meta
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Memos.Transaction as M
import qualified Penny.Copper.Flag as F
import qualified Penny.Copper.Number as N
import qualified Penny.Copper.Payees as P
import qualified Penny.Lincoln.Transaction as T
import Penny.Copper.Util (lexeme, eol, renMaybe, txtWords)
import qualified Penny.Lincoln.Transaction.Unverified as U

topLine ::
  DT.DefaultTimeZone
  -> Parser (U.TopLine Meta.TopLineMeta)
topLine dtz =
  f
  <$> M.memo
  <*> getPosition
  <*> lexeme (DT.dateTime dtz)
  <*> optionMaybe (lexeme F.flag)
  <*> optionMaybe (lexeme N.number)
  <*> optionMaybe (P.quotedPayee <|> P.unquotedPayee)
  <*  eol
  where
    f (me, tml) pos dt fl nu pa = tl where
      tl = U.TopLine dt fl nu pa me tlm
      tlm = Meta.TopLineMeta tml lin fn
      lin = Meta.TopLineLine . sourceLine $ pos
      fn = Meta.Filename . X.pack . sourceName $ pos


render ::
  DT.DefaultTimeZone
  -> T.TopLine m
  -> Maybe X.Text
render dtz tl =
  f
  <$> M.render (T.tMemo tl)
  <*> pure (DT.render dtz (T.tDateTime tl))
  <*> renMaybe (T.tFlag tl) F.render
  <*> renMaybe (T.tNumber tl) N.render
  <*> renMaybe (T.tPayee tl) P.smartRender
  where
    f meX dtX flX nuX paX =
      meX
      `X.append` (txtWords [dtX, flX, nuX, paX])
      `X.snoc` '\n'
