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
import qualified Penny.Lincoln as L

-- | Parse optional spaces, returns appropriate metadata.
spaces :: Parser L.SpaceBetween
spaces = f <$> many (char ' ') where
  f l = if null l then L.NoSpaceBetween else L.SpaceBetween

cmdtyQty :: Parser L.Commodity
            -> Parser (L.Amount, L.Format)
cmdtyQty p = let
  f c s q = (a, fmt) where
    a = L.Amount q c
    fmt = L.Format L.CommodityOnLeft s
  e = "amount, commodity on left"
  in f <$> p <*> spaces <*> Q.qty <?> e

lvl1CmdtyQty :: Parser (L.Amount, L.Format)
lvl1CmdtyQty = cmdtyQty C.quotedLvl1Cmdty

lvl3CmdtyQty :: Parser (L.Amount, L.Format)
lvl3CmdtyQty = cmdtyQty C.lvl3Cmdty

cmdtyOnRight :: Parser (L.Amount, L.Format)
cmdtyOnRight = let
  f q s c = (a, fmt) where
    a = L.Amount q c
    fmt = L.Format L.CommodityOnRight s
  e = "amount, commodity on right"
  in f
     <$> Q.qty
     <*> spaces
     <*> (C.quotedLvl1Cmdty <|> C.lvl2Cmdty)
     <?> e

-- | Parses an amount with its metadata. Handles all combinations of
-- commodities and quantities.
amount :: Parser (L.Amount, L.Format)
amount = lvl1CmdtyQty
         <|> lvl3CmdtyQty
         <|> cmdtyOnRight
         <?> "amount"

