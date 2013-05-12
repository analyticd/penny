module Penny.Copper.Gen.Parsers where

import Data.List (nubBy, genericSplitAt, genericReplicate)
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import Data.Text (pack, cons, snoc)
import qualified Penny.Copper.Gen.Terminals as T
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck (Gen, arbitrary)

--
-- * Accounts
--

lvl1SubAcct :: Gen (L.SubAccount, X.Text)
lvl1SubAcct = do
  txt <- fmap pack $ G.listOf1 T.lvl1AcctChar
  return (L.SubAccount txt, txt)

lvl1FirstSubAcct :: Gen (L.SubAccount, X.Text)
lvl1FirstSubAcct = lvl1SubAcct

lvl1OtherSubAcct :: Gen (L.SubAccount, X.Text)
lvl1OtherSubAcct = do
  (ac, gtxt) <- lvl1SubAcct
  return (ac, ':' `cons` gtxt)

lvl1Acct :: Gen (L.Account, X.Text)
lvl1Acct = do
  s1 <- lvl1SubAcct
  sr <- G.listOf lvl1OtherSubAcct
  let subs = fst s1 : fmap fst sr
      txts = X.concat . map snd $ s1:sr
  return (L.Account subs, txts)

quotedLvl1Acct :: Gen (L.Account, X.Text)
quotedLvl1Acct = fmap f lvl1Acct
  where
    f (ac, gTxt) = (ac, '{' `cons` gTxt `snoc` '}')

lvl2FirstSubAcct :: Gen (L.SubAccount, X.Text)
lvl2FirstSubAcct = do
  l1 <- T.letter
  ls <- fmap pack $ G.listOf T.lvl2AcctOtherChar
  let txt = l1 `cons` ls
  return (L.SubAccount txt, txt)

lvl2OtherSubAcct :: Gen (L.SubAccount, X.Text)
lvl2OtherSubAcct = do
  cs <- fmap pack $ G.listOf1 T.lvl2AcctOtherChar
  return (L.SubAccount cs, ':' `cons` cs)

lvl2Acct :: Gen (L.Account, X.Text)
lvl2Acct = do
  a1 <- lvl2FirstSubAcct
  as <- G.listOf lvl2OtherSubAcct
  let a = L.Account (map fst (a1:as))
      g = X.concat . map snd $ (a1:as)
  return (a, g)

ledgerAcct :: Gen (L.Account, X.Text)
ledgerAcct = G.oneof [quotedLvl1Acct, lvl2Acct]

--
-- * Commodities
--

data QuotedLvl1Cmdty = QuotedLvl1Cmdty L.Commodity X.Text
  deriving Show

data Lvl2Cmdty = Lvl2Cmdty L.Commodity X.Text
  deriving Show

data Lvl3Cmdty = Lvl3Cmdty L.Commodity X.Text
  deriving Show

data Cmdty
  = L1 QuotedLvl1Cmdty
  | L2 Lvl2Cmdty
  | L3 Lvl3Cmdty
  deriving Show

unwrapCmdty :: Cmdty -> (L.Commodity, X.Text)
unwrapCmdty c = case c of
  L1 (QuotedLvl1Cmdty y x) -> (y, x)
  L2 (Lvl2Cmdty y x) -> (y, x)
  L3 (Lvl3Cmdty y x) -> (y, x)

lvl1Cmdty :: Gen (L.Commodity, X.Text)
lvl1Cmdty = do
  cs <- fmap X.pack (G.listOf1 T.lvl1CmdtyChar)
  return (L.Commodity cs, cs)

quotedLvl1Cmdty :: Gen QuotedLvl1Cmdty
quotedLvl1Cmdty = fmap f lvl1Cmdty
  where
    f (c, gx) = QuotedLvl1Cmdty c ('"' `cons` gx `snoc` '"')

lvl2Cmdty :: Gen Lvl2Cmdty
lvl2Cmdty = do
  c1 <- T.lvl2CmdtyFirstChar
  cs <- fmap pack $ G.listOf T.lvl2CmdtyOtherChar
  return (Lvl2Cmdty (L.Commodity $ c1 `cons` cs) (c1 `cons` cs))

lvl3Cmdty :: Gen Lvl3Cmdty
lvl3Cmdty = do
  cs <- fmap pack $ G.listOf1 T.lvl3CmdtyChar
  return (Lvl3Cmdty (L.Commodity cs) cs)

genCmdty :: Gen Cmdty
genCmdty = G.oneof [ fmap L1 quotedLvl1Cmdty
                   , fmap L2 lvl2Cmdty
                   , fmap L3 lvl3Cmdty ]

uniqueCmdtys :: Gen [Cmdty]
uniqueCmdtys = G.sized $ \s -> do
  ls <- (G.resize (min s 3) $ G.listOf1 genCmdty)
  let f c1 c2 = (fst . unwrapCmdty $ c1) == (fst . unwrapCmdty $ c2)
  return (nubBy f ls)


interleave :: Gen (Maybe a) -> [a] -> Gen [a]
interleave g ls = case ls of
  [] -> return []
  a:[] -> return [a]
  a:as -> do
    r <- g
    rest <- interleave g as
    return $ case r of
      Nothing -> a:rest
      Just rt -> a:rt:rest

-- | Randomly add some thin spaces to a string. Does not always add
-- thin spaces at all.
addThinSpaces :: (String, Maybe String) -> Gen (String, Maybe String)
addThinSpaces (s, ms) = do
  addLeft <- Q.arbitrary
  addRight <- Q.arbitrary
  let genThin = G.elements [Nothing, Just '\x2009']
  l <- if addLeft then interleave genThin s else return s
  r <- case ms of
    Nothing -> return Nothing
    Just sr -> fmap Just $
               if addRight then interleave genThin sr else return sr
  return (l, r)

-- | Given the digits that are before and after the decimal point,
-- generate a base rendering. If q is a qty, then show q will always
-- return the same string. However, when being parsed from a file, a
-- text representation of that Qty might have small variations. For
-- example, 0.1 might also be entered as .1, or 123 might also be
-- entered as 123. (note that 123 cannot be entered as 123.0, as these
-- are the same number.) There might also be leading zeroes, as these
-- do not affect the number's final value (trailing zeroes do.) This
-- function returns a random generation that takes these variations
-- into account.
--
-- Returned is a pair. The first element is what to show to the left
-- of the decimal point. The second element is a Maybe, which is
-- Nothing if there is no decimal point and nothing after it, a Just
-- (empty string) if there is a decimal point, but nothing to show
-- after it, or a Just non-empty string if there is a decimal point
-- and something to show after it.
--
--  There will be no digit grouping.
baseRender
  :: (String, String)
  -> Gen (String, Maybe String)
baseRender (l, r) = do
  l' <- Q.sized $ \s -> do
          n <- G.oneof [return 0, G.choose (0, s)]
          return (replicate n '0' ++ l)
  r' <- case r of
    "" -> do
      showDot <- Q.arbitrary
      return $ if showDot then Just "" else Nothing
    _ -> return . Just $ r
  return (l', r')

quantity :: Gen (L.Qty, X.Text)
quantity = do
  q <- arbitrary
  x <- renderQtyWithThinSpaces q
  return (q, x)

renderQtyWithThinSpaces :: L.Qty -> Gen X.Text
renderQtyWithThinSpaces q = do
  let qd = qtyDigits q
  baseR <- baseRender qd
  withSpaces <- addThinSpaces baseR
  return $ renderQty withSpaces

-- | How many digits does this number have?
howManyDigits :: Integer -> Integer
howManyDigits i
  | i == 0 = 1
  | otherwise =
      let x = abs i
          go a = if x `div` (10 ^ a) == 0
                 then a
                 else go (a + 1)
      in go 0

-- | Given a Qty, return the digits that are before and after the
-- decimal point.
qtyDigits :: L.Qty -> (String, String)
qtyDigits q =
  let (m, p) = (L.mantissa q, L.places q)
      nd = howManyDigits m
      (l, r) = genericSplitAt (nd - p) (show m)
  in if p > nd
     then (l, genericReplicate (p - nd) '0' ++ r)
     else (l, r)

renderQty :: (String, Maybe String) -> X.Text
renderQty (l, mr) = (pack l) `X.append` r
  where
    r = case mr of
      Nothing -> X.empty
      Just rn -> '.' `X.cons` (X.pack rn)

