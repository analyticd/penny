module Penny.Copper.Price (price, render, unbox) where

import Text.Parsec ( char, getPosition, sourceLine, (<?>),
                     SourcePos )
import Text.Parsec.Text ( Parser )

import Control.Applicative ((<*>), (<*), (<|>), (<$))
import Data.Text (singleton, snoc, intercalate)
import qualified Data.Text as X

import qualified Penny.Lincoln.Boxes as Box
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Amount as A
import qualified Penny.Copper.Commodity as C
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Util (lexeme, eol)

{-
BNF-style specification for prices:

<price> ::= "dateTime" <fromCmdty> <toAmount>
<fromCmdty> ::= "quotedLvl1Cmdty" | "lvl2Cmdty"
<toAmount> ::= "amount"
-}

mkPrice :: SourcePos
         -> B.DateTime
         -> B.Commodity
         -> (B.Amount, M.Format)
         -> Maybe Box.PriceBox
mkPrice pos dt from (am, fmt) = let
  to = B.commodity am
  q = B.qty am
  pm = M.PriceMeta (Just pl) (Just fmt)
  pl = M.PriceLine . M.Line . sourceLine $ pos
  in do
    p <- B.newPrice (B.From from) (B.To to) (B.CountPerUnit q)
    return $ Box.PriceBox (B.PricePoint dt p) (Just pm)

maybePrice ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> Parser (Maybe Box.PriceBox)
maybePrice dtz rg =
  mkPrice
  <$ lexeme (char '@')
  <*> getPosition
  <*> lexeme (DT.dateTime dtz)
  <*> lexeme (C.quotedLvl1Cmdty <|> C.lvl2Cmdty)
  <*> A.amount rg
  <* eol
  <?> "price"
  
-- | A price with an EOL and whitespace after the EOL. @price d r@
-- will parse a PriceBox, where @d@ is the DefaultTimeZone for the
-- DateTime in the PricePoint, and @r@ is the radix point and grouping
-- character to parse the Amount. Fails if the price is not valid
-- (e.g. the from and to commodities are the same).
price ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> Parser Box.PriceBox
price dtz rg = do
  b <- maybePrice dtz rg
  case b of
    Nothing -> fail "invalid price given"
    Just p -> return p

-- | @render dtz rg f pp@ renders a price point @pp@. @dtz@ is the
-- DefaultTimeZone for rendering of the DateTime in the Price
-- Point. @rg@ is the radix point and grouping character for the
-- amount. @f@ is the Format for how the price will be
-- formatted. Fails if either the From or the To commodity cannot be
-- rendered.
render ::
  DT.DefaultTimeZone
  -> Q.GroupingSpec -- ^ Grouping to the left of the radix point
  -> Q.GroupingSpec -- ^ Grouping to the right of the radix point
  -> Q.RadGroup
  -> (B.PricePoint, M.Format)
  -> Maybe X.Text
render dtz gl gr rg (pp, fmt) = let
  dateTxt = DT.render dtz (B.dateTime pp)
  (B.From from) = B.from . B.price $ pp
  (B.To to) = B.to . B.price $ pp
  (B.CountPerUnit q) = B.countPerUnit . B.price $ pp
  mayFromTxt = C.renderLvl3 from <|> C.renderQuotedLvl1 from
  amt = B.Amount q to
  mayAmtTxt = A.render gl gr rg fmt amt
  in do
    amtTxt <- mayAmtTxt
    fromTxt <- mayFromTxt
    return $
       (intercalate (singleton ' ')
       [singleton '@', dateTxt, fromTxt, amtTxt])
       `snoc` '\n'

unbox :: Box.PriceBox -> Maybe (B.PricePoint, M.Format)
unbox b = do
  m <- Box.priceMeta b
  f <- M.priceFormat m
  return (Box.price b, f)
