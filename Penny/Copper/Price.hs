module Penny.Copper.Price where

import Control.Monad ( void )
import Text.Parsec ( char, many, getPosition, sourceLine, (<?>))
import qualified Text.Parsec.Pos as Pos
import Text.Parsec.Text ( Parser )

import Control.Applicative ((<$>), (<*>), (<**>), (*>), (<*),
                            (<|>), (<$))

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

mkPrice :: Pos.SourcePos
         -> B.DateTime
         -> B.Commodity
         -> (B.Amount, M.Format)
         -> Maybe Box.PriceBox
mkPrice pos dt from (am, fmt) = let
  to = B.commodity am
  q = B.qty am
  pm = M.PriceMeta pl fmt
  pl = M.PriceLine . M.Line . Pos.sourceLine $ pos
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
  
-- | A price with an EOL and whitespace after the EOL. Fails if the
-- price is not valid (e.g. the from and to commodities are the same).
price ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> Parser Box.PriceBox
price dtz rg = do
  b <- maybePrice dtz rg
  case b of
    Nothing -> fail "invalid price given"
    Just p -> return p
