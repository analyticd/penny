{-# LANGUAGE DeriveGeneric, DeriveFunctor, CPP #-}

-- | Containers for entries.
--
-- This module is the key guardian of the core principle of
-- double-entry accounting, which is that debits and credits must
-- always balance. An 'Ent' is a container for an 'Entry'. An 'Entry'
-- holds a 'DrCr' and an 'Amount' which, in turn, holds a 'Commodity'
-- and a 'Qty'. For a given 'Commodity' in a particular transaction,
-- the sum of the debits must always be equal to the sum of the
-- credits.
--
-- In addition to the 'Entry', the 'Ent' holds information about
-- whether the particular 'Entry' it holds is inferred or not. An Ent
-- is @inferred@ if the user did not supply the entry, but Penny was
-- able to deduce its 'Entry' because proper entries were supplied for
-- all the other postings in the transaction. The 'Ent' also holds
-- arbitrary metadata--which will typically be other information about
-- the particular posting, such as the payee, account, etc.
--
-- A collection of 'Ent' is an 'Ents'. This module will only create an
-- 'Ent' as part of an 'Ents' (though you can later separate the 'Ent'
-- from its other 'Ents' if you like.) In any given 'Ents', all of the
-- 'Ent' collectively have a zero balance.
--
-- This module also contains type synonyms used to represent a
-- Posting, which is an Ent bundled with its sibling Ents, and a
-- Transaction.

module Penny.Lincoln.Ents
  ( -- * Ent
    Inferred(..)
  , Ent
  , entry
  , inferred
  , meta

  -- * Ents
  , Ents
  , unEnts
  , tupleEnts
  , mapEnts
  , traverseEnts
  , ents
  , rEnts

  -- * Views
  , Posting(..)
  , Transaction(..)
  , transactionToPostings
  , headEnt
  , tailEnts
  , views
  , unrollSnd

#ifdef test
  , tests
#endif

  ) where

import Control.Applicative
import Control.Arrow (second)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Balance as Bal
import Control.Monad (guard)
import qualified Penny.Lincoln.Equivalent as Ev
import Penny.Lincoln.Equivalent ((==~))
import Data.Monoid (mconcat, (<>))
import Data.List (foldl', unfoldr, sortBy)
import Data.Maybe (isNothing, catMaybes)
import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fdbl

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
import qualified Data.Map as M
import qualified Penny.Lincoln.Bits.Qty as Qty

tests :: Test
tests = testGroup "Penny.Lincoln.Ents"
  [ testProperty "ents have at least two postings"
    $ QC.mapSize (min 10) (prop_twoPostings :: Ents QC.A -> Bool)

  , testProperty "genBalEntries generates balanced groups"
    $ QC.mapSize (min 10) prop_balEntries

  , testProperty "ents are always balanced"
    $ QC.mapSize (min 10) (prop_balanced :: Ents QC.A -> Bool)

  , testProperty "ents have no more than one inferred posting"
    $ QC.mapSize (min 10) (prop_inferred :: Ents QC.A -> Bool)

  , testProperty "genEntriesWithInfer creates inferable data"
    $ QC.mapSize (min 10) prop_genEntries

  , testProperty "ents function and NonRestricted behave properly"
    $ QC.mapSize (min 10) prop_ents

  , testProperty "NonRestricted makes two postings"
    $ QC.mapSize (min 10) prop_entsTwoPostings

  , testProperty "rEnts behaves as it should"
    $ QC.mapSize (min 10) prop_rEnts

  , testProperty "views gives as many views as there are postings"
    $ QC.mapSize (min 10) (prop_numViews :: Ents QC.A -> Bool)
  ]

#endif

data Inferred = Inferred | NotInferred
  deriving (Eq, Ord, Show, Generic)

instance Binary Inferred

data Ent m = Ent
  { entry :: B.Entry
  , inferred :: Inferred
  , meta :: m
  } deriving (Eq, Ord, Show, Generic)

-- | Two Ents are equivalent if the entries are equivalent and the
-- metadata is equivalent (whether the Ent is inferred or not is
-- ignored.)
instance Ev.Equivalent m => Ev.Equivalent (Ent m) where
  equivalent (Ent e1 _ m1) (Ent e2 _ m2) =
    e1 ==~ e2 && m1 ==~ m2
  compareEv (Ent e1 _ m1) (Ent e2 _ m2) =
    Ev.compareEv e1 e2 <> Ev.compareEv m1 m2

instance Functor Ent where
  fmap f (Ent e i m) = Ent e i (f m)

instance Binary m => Binary (Ent m)

newtype Ents m = Ents { unEnts :: [Ent m] }
  deriving (Eq, Ord, Show, Generic, Functor)

-- | Ents are equivalent if the content Ents of each are
-- equivalent. The order of the ents is insignificant.
instance Ev.Equivalent m => Ev.Equivalent (Ents m) where
  equivalent (Ents e1) (Ents e2) =
    let (e1', e2') = (sortBy Ev.compareEv e1, sortBy Ev.compareEv e2)
    in and $ (length e1 == length e2)
           : zipWith Ev.equivalent e1' e2'

  compareEv (Ents e1) (Ents e2) =
    let (e1', e2') = (sortBy Ev.compareEv e1, sortBy Ev.compareEv e2)
    in mconcat $ compare (length e1) (length e2)
               : zipWith Ev.compareEv e1' e2'

instance Fdbl.Foldable Ents where
  foldr f z (Ents ls) = case ls of
    [] -> z
    x:xs -> f (meta x) (Fdbl.foldr f z (map meta xs))

instance Tr.Traversable Ents where
  sequenceA = fmap Ents . Tr.sequenceA . map seqEnt . unEnts

mapEnts :: (Ent a -> b) -> Ents a -> Ents b
mapEnts f = Ents . map f' . unEnts where
  f' e = e { meta = f e }

traverseEnts :: Applicative f => (Ent a -> f b) -> Ents a -> f (Ents b)
traverseEnts f = fmap Ents . Tr.traverse f' . unEnts where
  f' en@(Ent e i _) = Ent <$> pure e <*> pure i <*> f en

seqEnt :: Applicative f => Ent (f a) -> f (Ent a)
seqEnt (Ent e i m) = Ent <$> pure e <*> pure i <*> m

tupleEnts :: Ents m -> (Ent m, Ent m, [Ent m])
tupleEnts (Ents ls) = case ls of
  t1:t2:ts -> (t1, t2, ts)
  _ -> error "tupleEnts: ents does not have two ents"

#ifdef test

instance Arbitrary a => Arbitrary (Ents a) where
  arbitrary = QC.oneof [ genNonRestricted
                       , genRestricted ]

-- | Ents always have at least two postings
prop_twoPostings :: Ents a -> Bool
prop_twoPostings (Ents ls) = length ls > 1

-- | Ents are always balanced
prop_balanced :: Ents a -> Bool
prop_balanced t = Bal.isBalanced bal == Bal.Balanced
  where
    bal = mconcat
          . map Bal.entryToBalance
          . map entry
          . unEnts
          $ t

-- | Ents contain no more than one inferred posting
prop_inferred :: Ents a -> Bool
prop_inferred t =
  (length . filter (== Inferred) . map inferred . unEnts $ t)
  < 2

-- | Information that should return a non-restricted transaction
newtype NonRestricted = NonRestricted
  { _unNonRestricted :: [(Maybe B.Entry, QC.A)] }
  deriving (Eq, Show)

instance Arbitrary NonRestricted where
  arbitrary = do
    let getEnt (ent, inf) =
          if inf == Inferred then Nothing else Just ent
    es <- fmap (map getEnt) $ genEntriesWithInfer
    metas <- QC.vector (length es)
    return $ NonRestricted (zip es metas)

newtype BalQtys = BalQtys { _unBalQtys :: ([B.Qty], [B.Qty]) }
  deriving (Eq, Show)

-- | Generates a group of balanced entries.
genBalEntries :: Gen ([B.Entry])
genBalEntries = do
  (_, qDeb, qCred) <- Qty.genBalQtys
  let qtysAndDrCrs = map (\en -> (B.Debit, en)) qDeb
                     ++ map (\en -> (B.Credit, en)) qCred
  cty <- arbitrary
  let mkEn (drCr, qty) = B.Entry drCr (B.Amount qty cty)
  return $ map mkEn qtysAndDrCrs

newtype BalEntries = BalEntries
  { unBalEntries :: [B.Entry] }
  deriving (Eq, Show)

instance Arbitrary BalEntries where
  arbitrary = fmap BalEntries genBalEntries

-- | genBalEntries generates groups that are balanced.
prop_balEntries :: BalEntries -> Bool
prop_balEntries
  = M.null
  . Bal.unBalance
  . Bal.removeZeroCommodities
  . mconcat
  . map Bal.entryToBalance
  . unBalEntries

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

-- | 'ents' makes ents as it should. Also tests whether
-- the 'Arbitrary' instance of 'NonRestricted' is behaving as it
-- should.

prop_ents :: NonRestricted -> Bool
prop_ents (NonRestricted ls) = isJust $ ents ls

-- | NonRestricted makes ents with two postings
prop_entsTwoPostings :: NonRestricted -> Bool
prop_entsTwoPostings (NonRestricted ls) = case ents ls of
  Nothing -> False
  Just t -> prop_twoPostings t

-- | 'rEnts' behaves as it should

prop_rEnts
  :: B.Commodity
  -> B.DrCr
  -> (B.Qty, QC.A)
  -> [(B.Qty, QC.A)]
  -> QC.A
  -> Bool
prop_rEnts c dc pr ls mt =
  let t = rEnts c dc pr ls mt
  in prop_twoPostings t && prop_balanced t && prop_inferred t

-- | Generates restricted ents
genRestricted :: Arbitrary a => Gen (Ents a)
genRestricted = liftM5 rEnts arbitrary arbitrary arbitrary
                                    arbitrary arbitrary

-- | Generates unrestricted ents. Assumes 'genEntriesWithInfer'
-- works as it should ('prop_ents' tests this.)
genNonRestricted :: Arbitrary a => Gen (Ents a)
genNonRestricted = do
  ls <- genEntriesWithInfer
  metas <- QC.vector (length ls)
  return . Ents $ zipWith (uncurry Ent) ls metas

#endif

instance Binary m => Binary (Ents m)

views :: Ents m -> [Ents m]
views = map Ents . orderedPermute . unEnts

-- | > unrollSnd (undefined, []) == []
--   > unrollSnd (1, [1,2,3]) = [(1,1), (1,2), (1,3)]

unrollSnd :: (a, [b]) -> [(a, b)]
unrollSnd = unfoldr f where
  f (_, []) = Nothing
  f (a, b:bs) = Just ((a, b), (a, bs))

-- | Splits a Transaction into Postings.
transactionToPostings :: Transaction -> [Posting]
transactionToPostings =
  map Posting . unrollSnd . second views . unTransaction

-- | Get information from the head posting in the View, which is the
-- one you are most likely interested in.
headEnt :: Ents m -> Ent m
headEnt (Ents ls) = case ls of
  [] -> error "ents: empty view"
  x:_ -> x

-- | Get information on sibling postings.
tailEnts :: Ents m -> (Ent m, [Ent m])
tailEnts (Ents ls) = case ls of
  [] -> error "ents: tailEnts: empty view"
  _:xs -> case xs of
    [] -> error "ents: tailEnts: only one sibling"
    s2:ss -> (s2, ss)

newtype Transaction = Transaction
  { unTransaction :: ( B.TopLineData, Ents B.PostingData ) }
  deriving (Eq, Show)

newtype Posting = Posting
  { unPosting :: ( B.TopLineData, Ents B.PostingData ) }
  deriving (Eq, Show)

#ifdef test

-- | 'views' gives as many views as there were postings

prop_numViews :: Ents m -> Bool
prop_numViews t = (length . views $ t) == (length . unEnts $ t)

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

ents
  :: [(Maybe B.Entry, m)]
  -> Maybe (Ents m)
ents ls = do
  guard . not . null $ ls
  let makePstg = makeEnt (inferredVal . map fst $ ls)
  fmap Ents $ mapM makePstg ls

rEnts
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
  -> Ents m
rEnts com dc (q1, m1) nonInfs lastMeta =
  let tot = foldl' B.add q1 . map fst $ nonInfs
      p1 = makePstg (q1, m1)
      ps = map makePstg nonInfs
      makePstg (q, m) = Ent (B.Entry dc (B.Amount q com))
                                NotInferred m
      lastPstg = Ent (B.Entry (B.opposite dc) (B.Amount tot com))
                         Inferred lastMeta
  in Ents $ p1:ps ++ [lastPstg]


-- | Changes Maybe Entries into Postings. Uses the inferred value if
-- the Maybe Entry is Nothing. If there is no inferred value, returns
-- Nothing.
makeEnt
  :: Maybe B.Entry
  -- ^ Inferred value
  -> (Maybe B.Entry, m)
  -> Maybe (Ent m)
makeEnt mayInf (mayEn, m) = case mayEn of
  Nothing -> case mayInf of
    Nothing -> Nothing
    Just inf -> return $ Ent inf Inferred m
  Just en -> return $ Ent en NotInferred m


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
