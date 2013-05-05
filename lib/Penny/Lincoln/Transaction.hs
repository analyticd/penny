{-# LANGUAGE DeriveGeneric, DeriveFunctor, CPP #-}

module Penny.Lincoln.Transaction
  ( Inferred(..)
  , Posting
  , pEntry
  , pInferred
  , pMeta
  , Transaction
  , unTransaction
  , mapPostings
  , transaction
  , rTransaction
  , View
  , ViewedPosting
  , unView
  , headPosting
  , views

#ifdef test
  , tests
#endif

  ) where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Balance as Bal
import Control.Monad (guard)
import Data.Monoid (mconcat)
import Data.List (foldl')
import Data.Maybe (isNothing, catMaybes)

#ifdef test
import Control.Monad (replicateM, liftM5)
import Data.Maybe (isJust)
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Gen, Arbitrary, arbitrary, (==>))
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck.Poly as QC
import qualified System.Random.Shuffle as Shuffle
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, testGroup)

tests :: Test
tests = testGroup "Penny.Lincoln.Transaction"
  [ testProperty "transactions have at least two postings"
    $ QC.mapSize (min 10) (prop_twoPostings :: Transaction QC.A -> Bool)

  , testProperty "transactions are always balanced"
    $ QC.mapSize (min 10) (prop_balanced :: Transaction QC.A -> Bool)

  , testProperty "transactions have no more than one inferred posting"
    $ QC.mapSize (min 10) (prop_inferred :: Transaction QC.A -> Bool)

  , testProperty "genEntriesWithInfer creates inferable data"
    $ QC.mapSize (min 10) prop_genEntries

  , testProperty "transactions function and NonRestricted behave properly"
    $ QC.mapSize (min 10) prop_transaction

  , testProperty "NonRestricted makes two postings"
    $ QC.mapSize (min 10) prop_transactionTwoPostings

  , testProperty "rTransaction behaves as it should"
    $ QC.mapSize (min 10) prop_rTransaction

  , testProperty "views gives as many views as there are postings"
    $ QC.mapSize (min 10) (prop_numViews :: Transaction QC.A -> Bool)
  ]

#endif

data Inferred = Inferred | NotInferred
  deriving (Eq, Ord, Show, Generic)

instance Binary Inferred

data Posting m = Posting
  { pEntry :: B.Entry
  , pInferred :: Inferred
  , pMeta :: m
  } deriving (Eq, Ord, Show, Generic)

instance Functor Posting where
  fmap f (Posting e i m) = Posting e i (f m)

instance Binary m => Binary (Posting m)

newtype Transaction m = Transaction { unTransaction :: [Posting m] }
  deriving (Eq, Ord, Show, Generic, Functor)

#ifdef test

instance Arbitrary a => Arbitrary (Transaction a) where
  arbitrary = QC.oneof [ genNonRestricted
                       , genRestricted ]

-- | Transactions always have at least two postings
prop_twoPostings :: Transaction a -> Bool
prop_twoPostings (Transaction ls) = length ls > 1

-- | Transactions are always balanced
prop_balanced :: Transaction a -> Bool
prop_balanced t = Bal.isBalanced bal == Bal.Balanced
  where
    bal = mconcat
          . map Bal.entryToBalance
          . map pEntry
          . unTransaction
          $ t

-- | Transactions contain no more than one inferred posting
prop_inferred :: Transaction a -> Bool
prop_inferred t =
  (length . filter (== Inferred) . map pInferred . unTransaction $ t)
  < 2

-- | Information that should return a non-restricted transaction
newtype NonRestricted = NonRestricted
  { _unNonRestricted :: [(Maybe B.Entry, QC.A)] }
  deriving (Eq, Show)

instance Arbitrary NonRestricted where
  arbitrary = do
    let getEnt (ent, inf) =
          if inf == Inferred then Nothing else Just ent
    ents <- fmap (map getEnt) $ genEntriesWithInfer
    metas <- QC.vector (length ents)
    return $ NonRestricted (zip ents metas)

-- | Generates a group of balanced quantities.
genBalQtys :: Gen ([B.Qty], [B.Qty])
genBalQtys = do
  total <- arbitrary
  group1alloc1 <- arbitrary
  group1allocRest <- QC.listOf arbitrary
  group2alloc1 <- arbitrary
  group2allocRest <- QC.listOf arbitrary
  let (g1r1, g1rs) = B.allocate total (group1alloc1, group1allocRest)
      (g2r1, g2rs) = B.allocate total (group2alloc1, group2allocRest)
  return $ (g1r1 : g1rs, g2r1 : g2rs)

-- | Generates a group of balanced entries.
genBalEntries :: Gen ([B.Entry])
genBalEntries = do
  (qDeb, qCred) <- genBalQtys
  let qtysAndDrCrs = map (\en -> (B.Debit, en)) qDeb
                     ++ map (\en -> (B.Credit, en)) qCred
  cty <- arbitrary
  let mkEn (drCr, qty) = B.Entry drCr (B.Amount qty cty)
  return $ map mkEn qtysAndDrCrs

-- | Generates a list of entries. At most, one of these is Inferred.
genEntriesWithInfer :: Gen [(B.Entry, Inferred)]
genEntriesWithInfer = do
  nGroups <- QC.suchThat QC.arbitrarySizedIntegral (> 0)
  entries <- fmap concat $ replicateM nGroups genBalEntries
  makeNothing <- arbitrary
  let entries' = if makeNothing
        then (head entries, Inferred)
             : map (\en -> (en, NotInferred)) (tail entries)
        else map (\en -> (en, NotInferred)) entries
  shuffle entries'


-- | genEntriesWithInfer is inferable
prop_genEntries :: QC.Property
prop_genEntries = QC.forAll genEntriesWithInfer $
  \ps -> Inferred `elem` (map snd ps)
         ==> isJust (inferredVal (map toEn ps))
  where
    toEn (en, inf) = if inf == Inferred then Nothing else Just en

-- | Shuffles a list.
shuffle :: [a] -> Gen [a]
shuffle ls = Gen.MkGen $ \g _ ->
  Shuffle.shuffle' ls (length ls) g

-- | 'transaction' makes transactions as it should. Also tests whether
-- the 'Arbitrary' instance of 'NonRestricted' is behaving as it
-- should.

prop_transaction :: NonRestricted -> Bool
prop_transaction (NonRestricted ls) = isJust $ transaction ls

-- | NonRestricted makes transactions with two postings
prop_transactionTwoPostings :: NonRestricted -> Bool
prop_transactionTwoPostings (NonRestricted ls) = case transaction ls of
  Nothing -> False
  Just t -> prop_twoPostings t

-- | 'rTransaction' behaves as it should

prop_rTransaction
  :: B.Commodity
  -> B.DrCr
  -> (B.Qty, QC.A)
  -> [(B.Qty, QC.A)]
  -> QC.A
  -> Bool
prop_rTransaction c dc pr ls mt =
  let t = rTransaction c dc pr ls mt
  in prop_twoPostings t && prop_balanced t && prop_inferred t

-- | Generates restricted transactions
genRestricted :: Arbitrary a => Gen (Transaction a)
genRestricted = liftM5 rTransaction arbitrary arbitrary arbitrary
                                    arbitrary arbitrary

-- | Generates unrestricted transactions. Assumes 'genEntriesWithInfer'
-- works as it should ('prop_transaction' tests this.)
genNonRestricted :: Arbitrary a => Gen (Transaction a)
genNonRestricted = do
  ls <- genEntriesWithInfer
  metas <- QC.vector (length ls)
  return . Transaction $ zipWith (uncurry Posting) ls metas

#endif

instance Binary m => Binary (Transaction m)

newtype View m = View { unView :: [Posting m] }
  deriving (Eq, Ord, Show, Functor)

views :: Transaction m -> [View m]
views = map View . orderedPermute . unTransaction

-- | Get information from the head posting in the View, which is the
-- one you are most likely interested in.
headPosting :: View m -> Posting m
headPosting (View ls) = case ls of
  [] -> error "transaction: empty view"
  x:_ -> x

type ViewedPosting = ( B.TopLineData, View B.PostingData )

#ifdef test

-- | 'views' gives as many views as there were postings

prop_numViews :: Transaction m -> Bool
prop_numViews t = (length . views $ t) == (length . unTransaction $ t)

#endif

-- | Returns a list of lists where each element in the original list
-- is in the front of a new list once.
--
-- > orderedPermute [1,2,3] == [[1,2,3], [2,3,1], [3,1,2]]
orderedPermute :: [a] -> [[a]]
orderedPermute ls = take (length ls) (iterate toTheBack ls)
  where
    toTheBack [] = []
    toTheBack (a:as) = as ++ [a]

mapPostings
  :: (Posting a -> Posting b)
  -> Transaction a
  -> Transaction b
mapPostings f = Transaction . map f . unTransaction

transaction
  :: [(Maybe B.Entry, m)]
  -> Maybe (Transaction m)
transaction ls = do
  guard . not . null $ ls
  let makePstg = makePosting (inferredVal . map fst $ ls)
  fmap Transaction $ mapM makePstg ls

rTransaction
  :: B.Commodity
  -- ^ Commodity for all postings
  -> B.DrCr
  -- ^ DrCr for all non-inferred postings
  -> (B.Qty, m)
  -- ^ Non-inferred posting 1
  -> [(B.Qty, m)]
  -- ^ Remaining non-inferred postings
  -> m
  -- ^ Metadata for inferred posting
  -> Transaction m
rTransaction com dc (q1, m1) nonInfs lastMeta =
  let tot = foldl' B.add q1 . map fst $ nonInfs
      p1 = makePstg (q1, m1)
      ps = map makePstg nonInfs
      makePstg (q, m) = Posting (B.Entry dc (B.Amount q com))
                                NotInferred m
      lastPstg = Posting (B.Entry (B.opposite dc) (B.Amount tot com))
                         Inferred lastMeta
  in Transaction $ p1:ps ++ [lastPstg]


-- | Changes Maybe Entries into Postings. Uses the inferred value if
-- the Maybe Entry is Nothing. If there is no inferred value, returns
-- Nothing.
makePosting
  :: Maybe B.Entry
  -- ^ Inferred value
  -> (Maybe B.Entry, m)
  -> Maybe (Posting m)
makePosting mayInf (mayEn, m) = case mayEn of
  Nothing -> case mayInf of
    Nothing -> Nothing
    Just inf -> return $ Posting inf Inferred m
  Just en -> return $ Posting en NotInferred m


-- | Gets a single inferred entry from a balance, if possible.
inferredVal :: [Maybe B.Entry] -> Maybe B.Entry
inferredVal ls = do
  guard ((length . filter id . map isNothing $ ls) == 1)
  let bal = mconcat
            . map Bal.entryToBalance
            . catMaybes
            $ ls
  case Bal.isBalanced bal of
    Bal.Inferable e -> Just e
    _ -> Nothing
