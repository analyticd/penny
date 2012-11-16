module PennyTest.Penny.Copper.Gen.Parsers where

import Control.Applicative ((<$>))
import Control.Monad (sequence)
import qualified Penny.Lincoln as L

import qualified PennyTest.Penny.Copper.Gen.Terminals as T
import qualified Data.Text as X
import Data.Text (Text, pack, snoc, cons)
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Gen (Gen)

lvl1SubAcct :: Gen (L.SubAccount, Gen X.Text)
lvl1SubAcct = do
  txt <- fmap pack $ G.listOf1 T.lvl1AcctChar
  return (L.SubAccount txt, return txt)

lvl1FirstSubAcct :: Gen (L.SubAccount, Gen X.Text)
lvl1FirstSubAcct = lvl1SubAcct

lvl1OtherSubAcct :: Gen (L.SubAccount, Gen X.Text)
lvl1OtherSubAcct = do
  (ac, gtxt) <- lvl1SubAcct
  txt <- gtxt
  return (ac, return $ ':' `cons` txt)

lvl1Acct :: Gen (L.Account, Gen X.Text)
lvl1Acct = do
  s1 <- lvl1SubAcct
  sr <- G.listOf lvl1OtherSubAcct
  let subs = fst s1 : fmap fst sr
      txts = fmap X.concat (sequence . map snd $ (s1:sr))
  return (L.Account subs, txts)

quotedLvl1Acct :: Gen (L.Account, Gen X.Text)
quotedLvl1Acct = fmap f lvl1Acct
  where
    f (ac, gTxt) = (ac, fmap (\x -> '{' `cons` x `snoc` '}') gTxt)

lvl2FirstSubAcct :: Gen (L.SubAccount, Gen X.Text)
lvl2FirstSubAcct = do
  l1 <- T.letter
  ls <- fmap pack $ G.listOf T.lvl2AcctOtherChar
  let txt = l1 `cons` ls
  return (L.SubAccount txt, return txt)

lvl2OtherSubAcct :: Gen (L.SubAccount, Gen X.Text)
lvl2OtherSubAcct = do
  cs <- fmap pack $ G.listOf1 T.lvl2AcctOtherChar
  return (L.SubAccount cs, return (':' `cons` cs))

lvl2Acct :: Gen (L.Account, Gen X.Text)
lvl2Acct = do
  a1 <- lvl2FirstSubAcct
  as <- G.listOf lvl2OtherSubAcct
  let a = L.Account (map fst (a1:as))
      g = fmap X.concat (sequence . map snd $ (a1:as))
  return (a, g)

ledgerAcct :: Gen (L.Account, Gen X.Text)
ledgerAcct = G.oneof [quotedLvl1Acct, lvl2Acct]

lvl1Cmdty :: Gen (L.Commodity, Gen X.Text)
lvl1Cmdty = do
  cs <- fmap X.pack (G.listOf1 T.lvl1CmdtyChar)
  return (L.Commodity cs, return cs)

quotedLvl1Cmdty :: Gen (L.Commodity, Gen X.Text)
quotedLvl1Cmdty = fmap f lvl1Cmdty
  where
    f (c, gx) = (c, fmap (\x -> '"' `cons` x `snoc` '"') gx)

lvl2Cmdty :: Gen (L.Commodity, Gen X.Text)
lvl2Cmdty = do
  c1 <- T.lvl2CmdtyFirstChar
  cs <- fmap pack $ G.listOf T.lvl2CmdtyOtherChar
  return (L.Commodity $ c1 `cons` cs, return (c1 `cons` cs))

lvl3Cmdty :: Gen (L.Commodity, Gen X.Text)
lvl3Cmdty = do
  cs <- fmap pack $ G.listOf1 T.lvl3CmdtyChar
  return (L.Commodity cs, return cs)

satisfy :: Gen a -> (a -> Maybe b) -> Gen b

