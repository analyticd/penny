module Penny.Copper.TopLine (
  topLine
  , render
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (<|>),
                            liftA, pure)
import qualified Data.Text as X
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

-- | Appends a space, but only if the Text is not null.
space :: X.Text -> X.Text
space x = if X.null x then x else x `X.snoc` ' '

-- | Takes a field that may or may not be present and a function that
-- renders it. If the field is not present at all, returns an empty
-- Text. Otherwise will succeed or fail depending upon whether the
-- rendering function succeeds or fails.
renMaybe :: Maybe a -> (a -> Maybe X.Text) -> Maybe X.Text
renMaybe mx f = case mx of
  Nothing -> Just X.empty
  Just a -> f a

render :: DT.DefaultTimeZone -> U.TopLine -> Maybe X.Text
render dtz (U.TopLine dt fl nu pa me) =
  f
  <$> pure (space (DT.render dtz dt))
  <*> (space <$> renMaybe fl F.render)
  <*> (space <$> renMaybe nu N.render)
  <*> renMaybe pa P.smartRender
  <*> M.render me
  where
    f meX dtX flX nuX paX =
      X.concat [meX, dtX, flX, nuX, paX] `X.snoc` '\n'
