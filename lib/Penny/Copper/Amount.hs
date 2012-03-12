-- | Amount parsers. An amount is a commodity and a quantity. (An
-- entry is an amount and a debit or credit).
--
-- Possible combinations:
--
-- * quoted Level 1 commodity, optional whitespace, quantity
--
-- * Level 3 commodity, optional whitespace, quantity
--
-- * Quantity, optional whitespace, quoted Level 1 commodity
--
-- * Quantity, optional whitespace, Level 2 commodity
--
-- Each quantity may be quoted or unquoted.
module Penny.Copper.Amount (
  amount
  , render
  ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Data.Text as X
import Text.Parsec ( char, many, (<?>) )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Commodity as C
import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Meta as M

-- | Parse optional spaces, returns appropriate metadata.
spaces :: Parser M.SpaceBetween
spaces = f <$> many (char ' ') where
  f l = if null l then M.NoSpaceBetween else M.SpaceBetween

cmdtyQty :: Parser B.Commodity
            -> Q.RadGroup
            -> Parser (B.Amount, M.Format)
cmdtyQty p rg = let
  f c s q = (a, fmt) where
    a = B.Amount q c
    fmt = M.Format M.CommodityOnLeft s
  e = "amount, commodity on left"
  in f <$> p <*> spaces <*> Q.qty rg <?> e

lvl1CmdtyQty :: Q.RadGroup -> Parser (B.Amount, M.Format)
lvl1CmdtyQty = cmdtyQty C.quotedLvl1Cmdty

lvl3CmdtyQty :: Q.RadGroup -> Parser (B.Amount, M.Format)
lvl3CmdtyQty = cmdtyQty C.lvl3Cmdty

cmdtyOnRight :: Q.RadGroup -> Parser (B.Amount, M.Format)
cmdtyOnRight rg = let  
  f q s c = (a, fmt) where
    a = B.Amount q c
    fmt = M.Format M.CommodityOnRight s
  e = "amount, commodity on right"
  in f
     <$> Q.qty rg
     <*> spaces
     <*> (C.quotedLvl1Cmdty <|> C.lvl2Cmdty)
     <?> e

-- | Parses an amount with its metadata. Handles all combinations of
-- commodities and quantities.
amount :: Q.RadGroup -> Parser (B.Amount, M.Format)
amount rg = lvl1CmdtyQty rg
            <|> lvl3CmdtyQty rg
            <|> cmdtyOnRight rg
            <?> "amount"

-- | Render an Amount. The Format is required so that the commodity
-- can be displayed in the right place.
render ::
  Q.GroupingSpec
  -- ^ Grouping to the left of the radix point
  -> Q.GroupingSpec
  -- ^ Grouping to the right of the radix point
  -> Q.RadGroup
  -> M.Format
  -> B.Amount
  -> Maybe X.Text
render gl gr rg f a = let
  (q, c) = (B.qty a, B.commodity a)
  qty = Q.quote $ Q.renderUnquoted rg gl gr q
  ws = case M.between f of
    M.SpaceBetween -> X.singleton ' '
    M.NoSpaceBetween -> X.empty
  mayLvl3 = C.renderLvl3 c
  mayLvl2 = C.renderLvl2 c
  in do
    quotedLvl1 <- C.renderQuotedLvl1 c
    let (l, r) = case M.side f of
          M.CommodityOnLeft -> case mayLvl3 of
            Nothing -> (quotedLvl1, qty)
            Just l3 -> (l3, qty)
          M.CommodityOnRight -> case mayLvl2 of
            Nothing -> (qty, quotedLvl1)
            Just l2 -> (qty, l2)
    return $ X.concat [l, ws, r]
