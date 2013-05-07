{-# LANGUAGE DeriveGeneric, DeriveFunctor, CPP #-}

module Penny.Lincoln.Ents
  ( Inferred(..)
  , Ent
  , entry
  , inferred
  , meta
  , Ents
  , unEnts
  , tupleEnts
  , mapEnts
  , ents
  , rEnts
  , View
  , Posting
  , Transaction
  , transactionToPostings
  , unView
  , headEnt
  , tailEnts
  , views
  , unrollSnd

#ifdef test
  , tests
#endif

  ) where

import Control.Arrow (second)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Balance as Bal
import Control.Monad (guard)
import Data.Monoid (mconcat)
import Data.List (foldl', unfoldr)
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
tests = testGroup "Penny.Lincoln.Ents"
  [ testProperty "ents have at least two postings"
    $ QC.mapSize (min 10) (prop_twoPostings :: Ents QC.A -> Bool)

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

instance Functor Ent where
  fmap f (Ent e i m) = Ent e i (f m)

instance Binary m => Binary (Ent m)

newtype Ents m = Ents { unEnts :: [Ent m] }
  deriving (Eq, Ord, Show, Generic, Functor)

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

newtype View m = View { unView :: [Ent m] }
  deriving (Eq, Ord, Show, Functor)

views :: Ents m -> [View m]
views = map View . orderedPermute . unEnts

-- | > unrollSnd (undefined, []) == []
--   > unrollSnd (1, [1,2,3]) = [(1,1), (1,2), (1,3)]

unrollSnd :: (a, [b]) -> [(a, b)]
unrollSnd = unfoldr f where
  f (_, []) = Nothing
  f (a, b:bs) = Just ((a, b), (a, bs))

-- | Splits a Transaction into Postings.
transactionToPostings :: Transaction -> [Posting]
transactionToPostings = unrollSnd . second views

-- | Get information from the head posting in the View, which is the
-- one you are most likely interested in.
headEnt :: View m -> Ent m
headEnt (View ls) = case ls of
  [] -> error "ents: empty view"
  x:_ -> x

-- | Get information on sibling postings.
tailEnts :: View m -> [Ent m]
tailEnts (View ls) = case ls of
  [] -> error "ents: tailEnts: empty view"
  _:xs -> xs

type Transaction = ( B.TopLineData, Ents B.PostingData )
type Posting = ( B.TopLineData, View B.PostingData )

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

mapEnts
  :: (Ent a -> Ent b)
  -> Ents a
  -> Ents b
mapEnts f = Ents . map f . unEnts

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
