module PennyTest.Lincoln.Transaction where

import Penny.Lincoln.Family (adopt, orphans)
import qualified Penny.Lincoln.Family.Family as Fam
import qualified Penny.Lincoln.Family.Siblings as Sib
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as B
import qualified PennyTest.Lincoln.Bits as TB
import PennyTest.Lincoln.Transaction.Unverified ()

import Control.Applicative (pure, (<$>), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import qualified Data.Traversable as Tr
import qualified System.Random as R
import qualified System.Random.Shuffle as Shuf
import Test.QuickCheck (Arbitrary, arbitrary, Gen, listOf1,
                        resize)
import qualified Test.QuickCheck.Gen as G
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, testGroup)

-- | All the data needed to generate a single unverified transaction.
data TransInputs =
  TransInputs {
    dateTime :: Gen B.DateTime
    , tFlag :: Gen (Maybe B.Flag)
    , tNumber :: Gen (Maybe B.Number)
    , tPayee :: Gen (Maybe B.Payee)
    , tMemo :: Gen B.Memo
    , pPayee :: Gen (Maybe B.Payee)
    , pNumber :: Gen (Maybe B.Number)
    , pFlag :: Gen (Maybe B.Flag)
    , pAccount :: Gen B.Account
    , pTags :: Gen B.Tags
    , pMemo :: Gen B.Memo
    , pCmdty :: Gen B.Commodity
    , pEntries :: Gen (TB.NEDrCrQty, TB.NEDrCrQty, Maybe TB.NEDrCrQty) }

-- | Data needed to generate a group of unverified Postings. (A single
-- Transaction might have more than one group of unverified Postings.)
data PGroupInputs =
  PGroupInputs {
    tgPayee :: Gen (Maybe B.Payee)
    , tgNumber :: Gen (Maybe B.Number)
    , tgFlag :: Gen (Maybe B.Flag)
    , tgAccount :: Gen B.Account
    , tgTags :: Gen B.Tags
    , tgMemo :: Gen B.Memo
    , tgCommodity :: Gen B.Commodity
    , tgEntries :: Gen (TB.NEDrCrQty, TB.NEDrCrQty, Maybe TB.NEDrCrQty) }

-- | All Postings generated can have entirely random data for these
-- elements.
data GenCommon = GenCommon {
  cPayee :: Gen (Maybe B.Payee)
  , cNumber :: Gen (Maybe B.Number)
  , cFlag :: Gen (Maybe B.Flag)
  , cTags :: Gen B.Tags
  , cMemo :: Gen B.Memo }

genCommon :: PGroupInputs -> GenCommon
genCommon t = GenCommon (tgPayee t) (tgNumber t) (tgFlag t) (tgTags t)
              (tgMemo t)

pGroupInputs :: TransInputs -> PGroupInputs
pGroupInputs t = PGroupInputs (pPayee t) (pNumber t) (pFlag t)
                 (pAccount t) (pTags t) (pMemo t) (pCmdty t)
                 (pEntries t)

-- | Generates a single unverified Posting.
makePosting ::
  GenCommon
  -> B.Account
  -> Maybe B.Entry
  -> Gen U.Posting
makePosting c ac en =
  U.Posting <$> cPayee c <*> cNumber c <*> cFlag c
  <*> pure ac <*> cTags c <*> pure en <*> cMemo c

-- | Generates a single unverified Posting with an Entry.
makePostingWithEntry ::
  GenCommon
  -> B.Account
  -> B.DrCr
  -> B.Commodity
  -> B.Qty
  -> Gen U.Posting
makePostingWithEntry c ac dc cy q = let
  am = B.Amount q cy
  en = Just $ B.Entry dc am
  in makePosting c ac en

-- | Generates a single unverified Posting with no Entry.
makePostingWithoutEntry ::
  GenCommon
  -> B.Account
  -> Gen U.Posting
makePostingWithoutEntry c ac = makePosting c ac Nothing

-- | Generates a single group of unverified Postings. Supply a
-- function to get the original and balancing quantities and DrCrs
-- from the generator. Whether the Postings are balanced or not
-- depends on the result of the function. Also returns the account
-- used to make the group of postings.
pGroupAlwaysSucceeds ::
  (Gen (TB.NEDrCrQty, TB.NEDrCrQty, Maybe TB.NEDrCrQty)
   -> Gen (TB.NEDrCrQty, TB.NEDrCrQty))
  -- ^ When applied to a generator, this function returns a pair p,
  -- where fst p are the original quantities and DrCrs, and snd p are
  -- the balancing quantities and DrCrs.
  -> PGroupInputs
  -> Gen (Sib.Siblings U.Posting, B.Account)
pGroupAlwaysSucceeds getQs p = do
  ac <- tgAccount p
  cy <- tgCommodity p
  (origs, bals) <- getQs . tgEntries $ p
  let mkP dc = makePostingWithEntry (genCommon p) ac dc cy
      mkPs (dc, qs) = Tr.traverse (mkP dc) qs
  originals <- mkPs origs
  balancers <- mkPs bals
  let (sib1:|rs1) = originals
      (sib2:|rs2) = balancers
      result = Sib.Siblings sib1 sib2 (rs1 ++ rs2)
  return (result, ac)

pGroupSometimesFails ::
  (Gen (TB.NEDrCrQty, TB.NEDrCrQty, Maybe TB.NEDrCrQty)
   -> Gen (Maybe (TB.NEDrCrQty, TB.NEDrCrQty)))
  -- ^ When applied to a generator, this function returns a pair p,
  -- where fst p are the original quantities and DrCrs, and snd p are
  -- the balancing quantities and DrCrs.
  -> PGroupInputs
  -> Gen (Maybe (Sib.Siblings U.Posting, B.Account))
pGroupSometimesFails getQs p = do
  ac <- tgAccount p
  cy <- tgCommodity p
  pair <- getQs . tgEntries $ p
  case pair of
    Just (origs, bals) -> do
      let mkP dc = makePostingWithEntry (genCommon p) ac dc cy
          mkPs (dc, qs) = Tr.traverse (mkP dc) qs
      originals <- mkPs origs
      balancers <- mkPs bals
      let (sib1:|rs1) = originals
          (sib2:|rs2) = balancers
          result = Sib.Siblings sib1 sib2 (rs1 ++ rs2)
      return $ Just (result, ac)
    Nothing -> return Nothing

-- | Generates a single balanced group of unverified Postings. (A
-- single Transaction might consist of one or more groups). This group
-- is balanced without inference.
pGroupNoInfer :: PGroupInputs -> Gen (Sib.Siblings U.Posting)
pGroupNoInfer p =
  fst <$> pGroupAlwaysSucceeds (fmap (\(f, s, _) -> (f, s))) p

-- | Generates a single balanced group of unverified Postings. (A
-- single Transaction might consist of one or more groups). This group
-- is balanced, but the balance must be inferred.
pGroupInfer :: PGroupInputs -> Gen (Maybe (Sib.Siblings U.Posting))
pGroupInfer p = do
  maybePair <- pGroupSometimesFails inferIfPossible p
  case maybePair of
    Nothing -> return Nothing
    Just (sibs, ac) -> do
      inferred <- makePostingWithoutEntry (genCommon p) ac
      let sibs' = Sib.Siblings (Sib.first sibs) (Sib.second sibs)
                  (Sib.rest sibs ++ [inferred])
      return $ Just sibs'

-- | Applied to a triple (a, b, c) where
--
-- * a is a Debit or Credit and a list of random quantities
--
-- * b is a Debit or Credit (the opposite of whatever was given in a)
-- and list of random quantities that add up to the sum of the
-- quantities given in a
--
-- * c is, if possible, a Debit or Credit (the opposite of whatever
-- was given in a) and a list of random quantities that add up to
-- something less than the sum of the quantities given in @a@
--
-- Returns a pair where fst is the original DrCr and random quanties,
-- and snd is balancing. If possible, snd adds up to less than a,
-- leading to a group that must be inferred. However, if this was not
-- possible, then a group that needs no inference is returned.
inferIfPossible ::
  Gen (TB.NEDrCrQty, TB.NEDrCrQty, Maybe TB.NEDrCrQty)
  -> Gen (Maybe (TB.NEDrCrQty, TB.NEDrCrQty))
inferIfPossible g = do
  (a, _, c) <- g
  case c of
    Nothing -> return Nothing
    Just r -> return $ Just (a, r)


-- | Makes unverified TopLines.
topLine :: TransInputs -> Gen U.TopLine
topLine t = U.TopLine
            <$> dateTime t
            <*> tFlag t
            <*> tNumber t
            <*> tPayee t
            <*> tMemo t

-- | Given a generator of unverified Postings, makes unverified
-- Transaction family.
unverifiedTransaction ::
  TransInputs
  -> (PGroupInputs -> Gen (Sib.Siblings U.Posting))
  -> Gen (Fam.Family U.TopLine U.Posting)
unverifiedTransaction t pf =
  adopt <$> topLine t <*> pf (pGroupInputs t)

-- | Makes a generator that makes multiple groups of balanced
-- unverified Postings. For a transaction to be balanced, it may
-- contain multiple groups of balanced unverified postings, but it can
-- contain only one group of inferrable postings.
multBalancedPostings :: PGroupInputs -> Gen (Sib.Siblings U.Posting)
multBalancedPostings p = toSibs <$> listOf1 (pGroupNoInfer p) where
  toSibs ls = let
    items = concat . fmap Foldable.toList $ ls
    in case items of
      (a:b:rs) -> Sib.Siblings a b rs
      _ -> error "multBalancedPostings error"

-- | Makes a generator that makes multiple groups of balanced
-- unverified Postings and that might also include inferrable
-- postings.
multBalancedPostingsWithInfer ::
  PGroupInputs
  -> Gen (Sib.Siblings U.Posting)
multBalancedPostingsWithInfer p = do
  balanced <- multBalancedPostings p
  doInferred <- arbitrary
  if doInferred
    then do inferred <- pGroupInfer p
            case inferred of
              Nothing -> return balanced
              Just inf ->
                let result = Sib.Siblings (Sib.first balanced)
                             (Sib.second balanced)
                             (Sib.rest balanced
                              ++ Foldable.toList inf)
                in return result
    else return balanced

-- | Shuffles a bunch of Postings.
shufflePostings :: Sib.Siblings U.Posting
                   -> Gen (Sib.Siblings U.Posting)
shufflePostings s = do
  let ls = Foldable.toList s
  shuffled <- shuffle ls
  let result = case shuffled of
        a:b:rs -> Sib.Siblings a b rs
        _ -> error "shufflePostings error"
  return result

-- | Generates unverified Transactions. Some may require inference,
-- some may not. Some may have multiple groups of balanced
-- Postings. All postings within a transaction will be shuffled.
randomUnverifiedTransactions ::
  TransInputs
  -> Gen (Fam.Family U.TopLine U.Posting)
randomUnverifiedTransactions t = do
  let shuffled pg = do
        ss <- multBalancedPostingsWithInfer pg
        shufflePostings ss
  unverifiedTransaction t shuffled
  
-- | Makes unverified Transactions that will likely not be renderable.
randomUnrenderable :: Gen (Fam.Family U.TopLine U.Posting)
randomUnrenderable = randomUnverifiedTransactions t where
  t = TransInputs arbitrary arbitrary arbitrary arbitrary arbitrary 
      arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary 
      arbitrary TB.randEntries

newtype RandomUnrenderable =
  RandomUnrenderable (Fam.Family U.TopLine U.Posting)
  deriving Show

instance Arbitrary RandomUnrenderable where
  arbitrary = RandomUnrenderable <$> resize 10 randomUnrenderable

-- | Gets the StdGen out of a Gen.
qcStdGen :: Gen R.StdGen
qcStdGen = G.MkGen $ \g _ -> g

-- | Shuffles a list.
shuffle :: [a] -> Gen [a]
shuffle ls = do
  g <- qcStdGen
  return $ Shuf.shuffle' ls (length ls) g

--
-- Tests
--

-- | A random unrenderable unverified transaction makes a
-- Transaction. Resizes the generator; without doing that, stack
-- overflows will result from the ridiculous amount of data. This may
-- be a bug, as Penny should be able to handle any amount of data in
-- theory, but of course whether this is a bug depends on whether the
-- problem is with Penny or with QuickCheck. (Penny's bugginess is
-- much more likely of course :)
prop_unrender :: RandomUnrenderable -> Bool
prop_unrender (RandomUnrenderable f) = case T.transaction f of
  Ex.Exception _ -> False
  Ex.Success _ -> True
  
test_unrender:: Test
test_unrender = testProperty s prop_unrender where
  s = "Random unrenderable unverified transactions make Transactions"

-- | Adding any unverified posting to an unverified but putatively
-- balanced transaction unbalances it. Adding a posting with no Entry
-- gives a CouldNotInfer error; adding a posting with an Entry gives
-- an UnbalancedError. Using this with a generator size much bigger
-- than 10 overflows the stack.
prop_unbalanced :: RandomUnrenderable -> U.Posting -> Bool
prop_unbalanced (RandomUnrenderable f) p = let
  expectedError = case U.entry p of
    Nothing -> T.CouldNotInferError
    Just _ -> T.UnbalancedError
  f' = Fam.Family (Fam.parent f) (Fam.child1 f) (Fam.child2 f)
       (p : Fam.children f)
  in case T.transaction f' of
    Ex.Success _ -> False
    Ex.Exception e -> e == expectedError

test_unbalanced :: Test
test_unbalanced = testProperty s prop_unbalanced where
  s = "Adding unverified postings unbalances"

-- | The balance of a random transaction is never empty.
prop_noEmptyBalances :: RandomUnrenderable -> Bool
prop_noEmptyBalances (RandomUnrenderable f) =
  case T.transaction f of
    Ex.Exception _ -> False
    Ex.Success t -> if Map.null (Bal.unBalance (transBalance t))
                    then False
                    else True

test_noEmptyBalances :: Test
test_noEmptyBalances = testProperty s prop_noEmptyBalances where
  s = "Balances of random transactions are never empty"

transBalance :: T.Transaction -> Bal.Balance
transBalance t = let
  entries = fmap (Bal.entryToBalance . T.pEntry)
            . orphans
            . T.unTransaction $ t
  tot = Bal.addBalances (Sib.first entries) (Sib.second entries)
  in foldr Bal.addBalances tot (Sib.rest entries)

-- | Every commodity in a random transaction's balance has a
-- BottomLine of Zero.
prop_bottomLineZero :: RandomUnrenderable -> Bool
prop_bottomLineZero (RandomUnrenderable u) =
  case T.transaction u of
    Ex.Exception _ -> False
    Ex.Success t -> let
      bal = Bal.unBalance . transBalance $ t
      p b = case b of
        Bal.Zero -> True
        _ -> False
      in Foldable.all p bal

test_bottomLineZero :: Test
test_bottomLineZero = testProperty s prop_bottomLineZero where
  s = "Bottom line of every commodity in transaction" 
      ++ " balance is zero"

tests :: Test
tests = testGroup "Transaction"
        [ test_unrender
        , test_unbalanced 
        , test_noEmptyBalances
        , test_bottomLineZero ]
