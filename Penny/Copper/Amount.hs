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
module Penny.Copper.Amount (amount) where

import Control.Applicative ((<$>), (<*>), pure, optional, (<|>))
import Control.Monad ( void )
import Text.Parsec ( char, choice, try, many, (<?>) )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Commodity as C
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Util (lexeme)
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
    fmt = M.Format c M.CommodityOnLeft s
  e = "amount, commodity on left"
  in f <$> p <*> spaces <*> Q.qty rg <?> e

lvl1CmdtyQty :: Q.RadGroup -> Parser (B.Amount, M.Format)
lvl1CmdtyQty = cmdtyQty C.quotedLvl1Cmdty

lvl3CmdtyQty :: Q.RadGroup -> Parser (B.Amount, M.Format)
lvl3CmdtyQty = cmdtyQty C.lvl3Cmdty

qtyCmdty :: Parser B.Commodity
            -> Q.RadGroup
            -> Parser (B.Amount, M.Format)
qtyCmdty p rg = let
  f q s c = (a, fmt) where
    a = B.Amount q c
    fmt = M.Format c M.CommodityOnRight s
  e = "amount, commodity on right"
  in f <$> Q.qty rg <*> spaces <*> p <?> e

qtyLvl1Cmdty :: Q.RadGroup -> Parser (B.Amount, M.Format)
qtyLvl1Cmdty = qtyCmdty C.quotedLvl1Cmdty

qtyLvl2Cmdty :: Q.RadGroup -> Parser (B.Amount, M.Format)
qtyLvl2Cmdty = qtyCmdty C.lvl2Cmdty

-- | Parses an amount with its metadata. Handles all combinations of
-- commodities and quantities.
amount :: Q.RadGroup -> Parser (B.Amount, M.Format)
amount rg = lvl1CmdtyQty rg
            <|> lvl3CmdtyQty rg
            <|> qtyLvl1Cmdty rg
            <|> qtyLvl2Cmdty rg
            <?> "amount"
