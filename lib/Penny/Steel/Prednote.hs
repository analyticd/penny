-- | Prednote - annotated predicates
--
-- Prednote allows you to annotate a predicate with a name. You can
-- then combine those predicates to form a new predicate. When using
-- the predicate to test particular subjects, you will get a tree that
-- can show you all successes and failures.

module Penny.Steel.Prednote
  ( -- * Simple interface
    -- ** Pdct
    Pdct(..)
  , PdctName

    -- *** Creating predicates
  , pdct

    -- *** Combining predicates
  , disjoin
  , (.||.)
  , conjoin
  , (.&&.)
  , expectFail
  , rename
  , (<?>)

    -- ** Test series
    -- *** Creating test series
  , SeriesGroup(..)
  , SeriesName
  , GroupName
  , seriesAtLeastN
  , eachSubjectMustBeTrue
  , group

    -- *** Running test series
  , Verbosity(..)
  , runSeries
  , showSeries
  , exitWithCode
  , SpaceCount

    -- * Innards

    -- | You can muck around in here if you want, though hopefully it
    -- should not be necessary for ordinary use.

    -- ** Types
  , Result
  , Info(..)
  , InfoTree
  , Passed
  , SeriesResult(..)
  , GroupResult(..)
  , SRInfo(..)
  , SeriesFn
  , Group(..)

    -- ** Showing
  , Indentation
  , showSeriesResult
  , showGroupResult
  , showSRInfo
  , showResult
  , showTree
  , printInColor
  , printResult
  , printPassed

  -- ** Pruning
  , filterTree
  , filterForest
  , pruneSeriesResult
  , pruneFailOnly
  , pruneBrief
  , pruneInteresting
  , pruneAllFails
  ) where

import Control.Applicative ((<*>), pure)
import Control.Arrow (second)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Tree as T
import qualified System.Console.Terminfo as TI
import qualified System.Exit as Exit

------------------------------------------------------------
-- Types
------------------------------------------------------------

type Result = Bool

type PdctName = String

data Info = Info
  { iResult :: Result
  , iName :: PdctName
  , interestingChildrenAre :: Bool
  }

type InfoTree = T.Tree Info
newtype Pdct a = Pdct { unPdct :: a -> InfoTree }

type SeriesName = String
type Passed = Bool

data SeriesResult a
  = SingleResult (SRInfo a)
  | SeveralResult (GroupResult a)

data GroupResult a = GroupResult GroupName [SeriesResult a]

data SRInfo a = SRInfo
  { srName :: SeriesName
  , srPassed :: Passed
  , srResults :: [(a, InfoTree)]
  }

type SeriesFn a = [a] -> SRInfo a

data Verbosity
  = Silent
    -- ^ Show nothing at all

  | FailOnly
    -- ^ Show only failing tests

  | Brief
  -- ^ Show only whether test succeeded or failed.

  | InterestingFails
  -- ^ Show interesting results from failed tests. Do not show results
  -- from succeeded tests.

  | AllFails
  -- ^ Show all resuls from failed tests. Do not show results from
  -- succeeded tests.

  | AllAll
  -- ^ Show all results from all tests, whether succeeded or failed.
  deriving Eq


type GroupName = String

-- | A group of Series. For ordinary use you will not need to use the
-- constructors; simply use the 'group' function as needed.
data SeriesGroup a
  = Single (SeriesFn a)
  | Several (Group a)

data Group a = Group GroupName [SeriesGroup a]

------------------------------------------------------------
-- Creating predicates
------------------------------------------------------------

-- | Creates a new Predicate with the given name.
pdct :: PdctName -> (a -> Bool) -> Pdct a
pdct d p = Pdct fn
  where
    fn pf = T.Node n []
      where
        n = Info (p pf) d True

------------------------------------------------------------
-- Combinators
------------------------------------------------------------

-- | Succeeds only when the given test fails.
expectFail :: Pdct a -> Pdct a
expectFail (Pdct t) = Pdct fn
  where
    d = "boolean not"
    fn s = T.Node n [c]
      where
        c@(T.Node (Info rslt _ _) _) = t s
        n = Info (not rslt) d True

-- | Renames a predicate.
rename :: PdctName -> Pdct a -> Pdct a
rename d (Pdct t) = Pdct fn
  where
    fn s = T.Node l' cs
      where
        T.Node l cs = t s
        l' = l { iName = d }

-- | Operator version of 'rename'.
(<?>) :: PdctName -> Pdct a -> Pdct a
(<?>) = rename
infix 0 <?>

-- | Disjunction: succeeds unless all argument tests fail.  Fails if
-- test list is empty.
disjoin :: [Pdct a] -> Pdct a
disjoin ls = Pdct fn
  where
    fn pf = T.Node (Info rslt "disjunction" False) fs
      where
        fs = map unPdct ls <*> pure pf
        rslt = any (iResult . T.rootLabel) fs

-- | Operator version of 'disjoin', uses a generic name.
(.||.) :: Pdct a -> Pdct a -> Pdct a
(.||.) x y = disjoin [x, y]

infixr 2 .||.

-- | Conjunction: succeeds if all are Good.  Succeeds if test list is
-- empty.
conjoin :: [Pdct a] -> Pdct a
conjoin ls = Pdct fn
  where
    fn pf = T.Node (Info rslt "conjunction" False) fs
      where
        fs = map unPdct ls <*> pure pf
        rslt = all (iResult . T.rootLabel) fs


-- | Operator version of conjoin, uses a generic description.
(.&&.) :: Pdct a -> Pdct a -> Pdct a
(.&&.) x y = conjoin [x, y]

infixr 3 .&&.

------------------------------------------------------------
-- Series
------------------------------------------------------------

-- | Passes if at least n subjects are True.
seriesAtLeastN
  :: SeriesName
  -> Int
  -> Pdct a
  -> SeriesGroup a
seriesAtLeastN name i (Pdct t) = Single fn
  where
    fn pfs = SRInfo name atLeast pairs
      where
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
        atLeast = ( length
                    . filter id
                    . map (iResult . T.rootLabel)
                    $ rslts
                  ) >= i

-- | Every subject is run through the test. Each subject must return
-- True; otherwise the test fails.
eachSubjectMustBeTrue
  :: SeriesName
  -> Pdct a
  -> SeriesGroup a
eachSubjectMustBeTrue n (Pdct t) = Single fn
  where
    fn pfs = SRInfo n passed pairs
      where
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
        passed = all (iResult . T.rootLabel) rslts

group :: GroupName -> [SeriesGroup a] -> SeriesGroup a
group n ts = Several $ Group n ts

runSeries :: [a] -> SeriesGroup a -> SeriesResult a
runSeries pfs t = runItemTest pfs t
  where
    runItemTest ps i = case i of
      Single tl -> SingleResult (tl ps)
      Several g -> SeveralResult (runGroupTest g ps)
    runGroupTest (Group n is) ps =
      GroupResult n (map (runItemTest ps) is)

------------------------------------------------------------
-- Showing
------------------------------------------------------------

type Indentation = Int


showSeriesResult
  :: TI.Terminal
  -> (a -> String)
  -> Indentation
  -> SpaceCount
  -> SeriesResult a
  -> IO ()
showSeriesResult t swr i sc sr = case sr of
  SingleResult si -> showSRInfo t swr i sc si
  SeveralResult gr -> showGroupResult t swr i sc gr

showGroupResult
  :: TI.Terminal
  -> (a -> String)
  -> Indentation
  -> SpaceCount
  -> GroupResult a
  -> IO ()

showGroupResult t swr i sc (GroupResult n ts) = do
  putStr (replicate (i * sc) ' ')
  putStrLn n
  mapM_ (showSeriesResult t swr i sc) ts

type SpaceCount = Int

showSRInfo
  :: TI.Terminal
  -> (a -> String)
  -> Indentation
  -> SpaceCount
  -> SRInfo a
  -> IO ()
showSRInfo t swr i sc (SRInfo n p rs) = do
  putStr (replicate (i * sc) ' ')
  printPassed t p
  putStrLn $ " " ++ n
  mapM_ (showResult t swr (i + 1) sc) rs

showResult
  :: TI.Terminal
  -> (a -> String)
  -> Indentation
  -> SpaceCount
  -> (a, InfoTree)
  -> IO ()
showResult t swr i sc (a, T.Node (Info r n _) cs) = do
  putStr (replicate (i * sc) ' ')
  printResult t r
  putStrLn $ " " ++ n ++ " - " ++ swr a
  mapM_ (showTree t (i + 1) sc) cs

showTree
  :: TI.Terminal
  -> Indentation
  -> SpaceCount
  -> InfoTree
  -> IO ()
showTree t i sc (T.Node (Info r n _) cs) = do
  putStr (replicate (i * sc) ' ')
  printResult t r
  putStrLn $ " " ++ n
  mapM_ (showTree t (i + 1) sc) cs

printInColor :: TI.Terminal -> TI.Color -> String -> IO ()
printInColor t c s =
  case TI.getCapability t TI.withForegroundColor of
    Nothing -> putStr s
    Just cap -> TI.runTermOutput t (cap c (TI.termText s))

printResult :: TI.Terminal -> Result -> IO ()
printResult t r = do
  putStr "["
  if r
    then do
      printInColor t TI.Green "TRUE"
      putStr "] "
    else do
      printInColor t TI.Red "FALSE"
      putStr "]"

printPassed :: TI.Terminal -> Passed -> IO ()
printPassed t p = do
  putStr "["
  if p
    then printInColor t TI.Green " OK "
    else printInColor t TI.Red "FAIL"
  putStr "]"

showSeries
  :: TI.Terminal
  -> (a -> String)
  -> SpaceCount
  -> Verbosity
  -> SeriesResult a
  -> IO ()
showSeries ti swr sc v =
  fromMaybe (return ())
  . fmap (showSeriesResult ti swr 0 sc)
  . pruneSeriesResult v


------------------------------------------------------------
-- Pruning
------------------------------------------------------------

filterTree
  :: InfoTree
  -> InfoTree
filterTree (T.Node l ts) =
  T.Node l (filterForest (interestingChildrenAre l) ts)

filterForest
  :: Bool
  -> T.Forest Info
  -> T.Forest Info
filterForest b = filter ((== b) . (iResult . T.rootLabel))
                 . map filterTree

pruneSeriesResult :: Verbosity -> SeriesResult a -> Maybe (SeriesResult a)
pruneSeriesResult v tr = case v of
  Silent -> Nothing
  FailOnly -> pruneFailOnly tr
  Brief -> Just $ pruneBrief tr
  InterestingFails -> Just $ pruneInteresting tr
  AllFails -> Just $ pruneAllFails tr
  AllAll -> Just tr

pruneFailOnly :: SeriesResult a -> Maybe (SeriesResult a)
pruneFailOnly tr = case tr of
  SingleResult sr -> pruneSingleResult sr
  SeveralResult gr -> pruneSeveralResult gr
  where
    pruneSingleResult (SRInfo n p _) =
      if p
      then Nothing
      else Just (SingleResult (SRInfo n p []))
    pruneSeveralResult (GroupResult n ls) =
      case mapMaybe pruneFailOnly ls of
        [] -> Nothing
        xs -> Just (SeveralResult (GroupResult n xs))

pruneBrief :: SeriesResult a -> SeriesResult a
pruneBrief tr = case tr of
  SingleResult sr -> pruneSingleResult sr
  SeveralResult gr -> pruneSeveralResult gr
  where
    pruneSingleResult (SRInfo n p _) =
      SingleResult (SRInfo n p [])
    pruneSeveralResult (GroupResult n ls) =
      SeveralResult (GroupResult n (map pruneBrief ls))

pruneInteresting :: SeriesResult a -> SeriesResult a
pruneInteresting tr = case tr of
  SingleResult sr -> pruneSingleResult sr
  SeveralResult gr -> pruneSeveralResult gr
  where
    pruneSingleResult (SRInfo n p ls) =
      SingleResult (SRInfo n p (map (second filterTree) ls))
    pruneSeveralResult (GroupResult n ls) =
      SeveralResult (GroupResult n (map pruneInteresting ls))

pruneAllFails :: SeriesResult a -> SeriesResult a
pruneAllFails tr = case tr of
  SingleResult sr -> pruneSingleResult sr
  SeveralResult gr -> pruneSeveralResult gr
  where
    pruneSingleResult (SRInfo n p ls) =
      let ls' = if p then [] else ls
      in SingleResult (SRInfo n p ls')
    pruneSeveralResult (GroupResult n ls) =
      SeveralResult (GroupResult n (map pruneAllFails ls))


--
-- Old
--

exitWithCode :: [SeriesResult a] -> IO ()
exitWithCode srs =
  if and . concatMap getList $ srs
  then Exit.exitSuccess
  else Exit.exitFailure
  where
    getList res = case res of
      SingleResult (SRInfo _ p _) -> [p]
      SeveralResult (GroupResult _ rs) -> concatMap getList rs

