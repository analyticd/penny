module PennyTest.Penny.Copper.Gen.Parsers where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (genericLength, genericSplitAt, genericReplicate)
import qualified Penny.Lincoln as L

import qualified PennyTest.Penny.Copper.Gen.Terminals as T
import qualified Data.Text as X
import Data.Text (pack, snoc, cons)
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Property as P
import qualified Test.QuickCheck as Q

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

lvl1Cmdty :: Gen (L.Commodity, X.Text)
lvl1Cmdty = do
  cs <- fmap X.pack (G.listOf1 T.lvl1CmdtyChar)
  return (L.Commodity cs, cs)

quotedLvl1Cmdty :: Gen (L.Commodity, X.Text)
quotedLvl1Cmdty = fmap f lvl1Cmdty
  where
    f (c, gx) = (c, '"' `cons` gx `snoc` '"')

lvl2Cmdty :: Gen (L.Commodity, X.Text)
lvl2Cmdty = do
  c1 <- T.lvl2CmdtyFirstChar
  cs <- fmap pack $ G.listOf T.lvl2CmdtyOtherChar
  return (L.Commodity $ c1 `cons` cs, c1 `cons` cs)

lvl3Cmdty :: Gen (L.Commodity, X.Text)
lvl3Cmdty = do
  cs <- fmap pack $ G.listOf1 T.lvl3CmdtyChar
  return (L.Commodity cs, cs)

-- | Generates integers.
integers :: Gen (L.Mantissa, L.Places)
integers = (\m -> (m, 0))
           <$> G.choose (1, fromIntegral (maxBound :: Int))

-- | How many digits does this number have?
numOfDigits :: Integer -> Integer
numOfDigits = genericLength . show

-- | Generates very small numbers.
verySmall :: Gen (L.Mantissa, L.Places)
verySmall = do
  m <- G.choose (1, fromIntegral (maxBound :: Int))
  let nd = numOfDigits m
  p <- G.choose (nd * 10, nd * 100)
  return (m, p)

-- | Generates numbers where the exponent is equal to the number of
-- digits (e.g. .345).
expNumOfDigits :: Gen (L.Mantissa, L.Places)
expNumOfDigits = G.sized $ \s -> do
  m <- G.choose (1, max 1 (fromIntegral s))
  return (m, numOfDigits m)

-- | Generates numbers that depend on size parameter. The exponent has
-- up to five more places than in the number generated.
sizedQty :: Gen (L.Mantissa, L.Places)
sizedQty = G.sized $ \s -> do
  m <- G.choose (1, max 1 (fromIntegral (s ^ (3 :: Int))))
  p <- G.choose (0, numOfDigits m + 5)
  return (m, p)

-- | Generates large numbers.
large :: Gen (L.Mantissa, L.Places)
large = do
  m <- G.choose (10 ^ (7 :: Int), (fromIntegral (maxBound :: Int)))
  p <- G.choose (0, 4)
  return (m, p)


-- | Generates typical numbers.
typical :: Gen (L.Mantissa, L.Places)
typical = (,)
          <$> G.choose (1, 10 ^ (7 :: Int))
          <*> G.choose (0, 4)

mkQty
  :: Gen (L.Mantissa, L.Places)
  -> Ex.ExceptionalT P.Result Gen L.Qty
mkQty g = do
  (m, p) <- lift g
  case L.newQty m p of
    Nothing -> Ex.throwT P.failed { P.reason = e }
      where e = "failed to make Qty."
    Just q -> return q

-- | Given a Qty, return the digits that are before and after the
-- decimal point.
qtyDigits :: L.Qty -> (String, String)
qtyDigits q =
  let (m, p) = (L.mantissa q, L.places q)
      nd = numOfDigits m
      (l, r) = genericSplitAt (nd - p) (show m)
  in if p > nd
     then (l, genericReplicate (p - nd) '0' ++ r)
     else (l, r)


-- | Given the digits that are before and after the decimal point,
-- generate a base rendering. If q is a qty, then show q will always
-- return the same string. However, when being parsed from a file, a
-- text representation of that Qty might have small variations. For
-- example, 0.1 might also be entered as .1, or 123 might also be
-- entered as 123. (note that 123 cannot be entered as 123.0, as these
-- are the same number.) There might also be leading zeroes, as these
-- do not affect the number's final value (trailing zeroes do.) This
-- function returns a random generation that takes these variations
-- into account. There will be no digit grouping.
baseRender :: (String, String) -> Ex.ExceptionalT P.Result Gen String
baseRender (l, r) = do
  l' <- lift $ Q.sized $ \s -> do
          n <- G.oneof [return 0, G.choose (0, s)]
          return (replicate n '0' ++ l)
  r' <- case r of
    "" -> do
      showDot <- lift Q.arbitrary
      return $ if showDot then "." else ""
    _ -> return $ '.' : r
  case (l, r) of
    ("", "") -> Ex.throwT P.failed { P.reason = e }
      where e = "quantity rendering has no digits."
    _ -> return $ l' ++ r'



-- | Randomly add some thin spaces to a string.
addThinSpaces :: String -> Gen String
addThinSpaces = undefined
