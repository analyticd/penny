-- | Prednote - annotated predicates
--
-- Prednote allows you to annotate a predicate with a name. You can
-- then combine those predicates to form a new predicate. When using
-- the predicate to test particular subjects, you will get a tree that
-- can show you all successes and failures.

module Penny.Steel.Prednote
  ( -- * Pdct
    Pdct
  , PdctName

    -- ** Creating predicates
  , pdct
  , true
  , false

    -- ** Combining predicates
  , disjoin
  , (.||.)
  , conjoin
  , (.&&.)
  , expectFail
  , rename
  , (<?>)

    -- * Test series
    -- ** Creating test series
  , SeriesGroup
  , SeriesName
  , GroupName
  , seriesAtLeastN
  , eachSubjectMustBeTrue
v  , processTrueSubjects
  , group

    -- ** Running test series
  , Verbosity(..)
  , PassVerbosity
  , FailVerbosity
  , Result
  , SpaceCount
  , Passed
  , runSeries
  , showSeries
  , resultList

  ) where

import Control.Arrow (second)
import Control.Applicative ((<*>), pure)
import Data.Maybe (mapMaybe)
import qualified Data.Tree as T
import qualified System.Console.Terminfo as TI

------------------------------------------------------------
-- Types
------------------------------------------------------------

--
-- Predicates
--

type PdctResult = Bool

type PdctName = String

data Info = Info
  { iResult :: PdctResult
  , iName :: PdctName
  , iInterestingIf :: Bool
  }

type InfoTree = T.Tree Info
newtype Pdct a = Pdct { unPdct :: a -> InfoTree }

--
-- Series
--
type SeriesFn a = [a] -> RInfo a
type SeriesNode a = Either (SeriesFn a) GroupName

newtype SeriesGroup a
  = SeriesGroup { unSeriesGroup :: T.Tree (SeriesNode a) }

--
-- Results
--

type Passed = Bool

type SeriesName = String
data RInfo a = RInfo
  { _srName :: SeriesName
  , srPassed :: Passed
  , _srResults :: [(a, InfoTree)]
  }

data FRInfo a = FRInfo
  { _frName :: SeriesName
  , _frPassed :: Passed
  , _frResults :: [(a, T.Tree (ShowItem, Info))]
  }

type ShowItem = Bool
type GroupName = String
type RNode a = Either (RInfo a) GroupName
type FRNode a = (ShowItem, Either (FRInfo a) GroupName)

data PdctInfo = PdctInfo
  { _piResult :: PdctResult
  , _piName :: PdctName
  , _piInterestingIf :: Bool
  , _piLevel :: Int
  }

data NumInfo a = NumInfo
  { _niLevel :: Int
  , _niShowNode :: ShowItem
  , _niName :: SeriesName
  , _niPassed :: Passed
  , _niResults :: [(a, T.Tree PdctInfo)]
  }

type NumNode a = Either (NumInfo a) GroupName

--
-- Showing results
--

data Verbosity
  = Silent
  | Status
  | Interesting
  | All
  deriving (Eq, Ord, Show)

type PassVerbosity = Verbosity
type FailVerbosity = Verbosity
type SpaceCount = Int

------------------------------------------------------------
-- Creating predicates
------------------------------------------------------------

-- | Creates a new Predicate with the given name.
pdct :: PdctName -> (a -> Bool) -> Pdct a
pdct d p = Pdct fn
  where
    fn pf = T.Node n []
      where
        n = Info (p pf) d False

-- | Always returns True.
true :: Pdct a
true = Pdct fn
  where
    fn _ = T.Node n []
      where
        n = Info True "always True" False

-- | Always returns False.
false :: Pdct a
false = Pdct fn
  where
    fn _ = T.Node n []
      where
        n = Info False "always False" True

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
        n = Info (not rslt) d False

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
seriesAtLeastN name i (Pdct t) = SeriesGroup $ T.Node (Left fn) []
  where
    fn pfs = RInfo name atLeast pairs
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
eachSubjectMustBeTrue n (Pdct t) = SeriesGroup $ T.Node (Left fn) []
  where
    fn pfs = RInfo n passed pairs
      where
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
        passed = all (iResult . T.rootLabel) rslts

-- | Every subject is run through the test. Subjects that return True
-- are then fed to the given function. The result of the function is
-- the result of the test.
processTrueSubjects
  :: SeriesName
  -> ([a] -> Bool)
  -> Pdct a
  -> SeriesGroup a
processTrueSubjects name fp (Pdct t) =
  SeriesGroup $ T.Node (Left fn) []
  where
    fn pfs = RInfo name r pairs
      where
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
        r = fp . map fst . filter (iResult . T.rootLabel . snd) $ pairs

group :: GroupName -> [SeriesGroup a] -> SeriesGroup a
group n ts = SeriesGroup $ T.Node (Right n) (map unSeriesGroup ts)

runSeries :: [a] -> SeriesGroup a -> Result a
runSeries pfs (SeriesGroup t) = Result $ fmap (toRNode pfs) t

toRNode :: [a] -> SeriesNode a -> RNode a
toRNode as n = case n of
  Left fn -> Left $ fn as
  Right s -> Right s

------------------------------------------------------------
-- Showing
------------------------------------------------------------

numberLevels :: Int -> (Int -> a -> b) -> T.Tree a -> T.Tree b
numberLevels i f (T.Node l cs) =
  T.Node (f i l) (map (numberLevels' (i + 1) f) cs)

type Indentation = Int

showResult
  :: TI.Terminal
  -> (a -> String)
  -> SpaceCount
  -> Indentation
  -> Result a
  -> IO ()
showResult term swr sc ind r = do
  let nrs = numberLevels ind . unResult $ r
  flat <- return $! T.flatten nrs
  mapM_ (showNode term swr sc) flat

showNode
  :: TI.Terminal
  -> (a -> String)
  -> SpaceCount
  -> (Indentation, RNode a)
  -> IO ()
showNode term swr sc (ind, nd) = case nd of
  Right s -> putStrLn (replicate (ind * sc) ' ' ++ s)
  Left info -> showRInfo term swr ind sc info

showRInfo
  :: TI.Terminal
  -> (a -> String)
  -> Indentation
  -> SpaceCount
  -> RInfo a
  -> IO ()
showRInfo t swr i sc (RInfo n p rs) = do
  putStr (replicate (i * sc) ' ')
  printPassed t p
  putStrLn $ " " ++ n
  mapM_ (showPair t sc swr (i + 1)) rs

showInfo
  :: TI.Terminal
  -> SpaceCount
  -> (Indentation, Info)
  -> IO ()
showInfo term sc (ind, Info rslt s _) = do
  putStr (replicate (ind * sc) ' ')
  printResult term rslt
  putStrLn s

showInfoTree
  :: TI.Terminal
  -> SpaceCount
  -> Indentation
  -> InfoTree
  -> IO ()
showInfoTree term sc ind =
  mapM_ (showInfo term sc)
  . T.flatten
  . numberLevels ind

showPair
  :: TI.Terminal
  -> SpaceCount
  -> (a -> String)
  -> Indentation
  -> (a, InfoTree)
  -> IO ()
showPair term sc swr ind (a, T.Node (Info r n _) cs) = do
  putStr (replicate (ind * sc) ' ')
  printResult term r
  putStrLn $ " " ++ n ++ " - " ++ swr a
  mapM_ (showInfoTree term sc (ind + 1)) cs


--
-- Colors
--

printInColor :: TI.Terminal -> TI.Color -> String -> IO ()
printInColor t c s =
  case TI.getCapability t TI.withForegroundColor of
    Nothing -> putStr s
    Just cap -> TI.runTermOutput t (cap c (TI.termText s))

printResult :: TI.Terminal -> PdctResult -> IO ()
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
  -> Result a
  -> IO ()
showSeries ti swr sc v sr = case pruneResult v sr of
  Nothing -> return ()
  Just sr' -> showResult ti swr sc 0 sr'


------------------------------------------------------------
-- Pruning
------------------------------------------------------------

-- | Descends through a tree. If the first parameter is False, this
-- node and all its children will be marked as False. If the first
-- parameter is True, this node will be marked True if the predicate
-- is True, or False (and all its children false) if it is false.
flagTree :: Bool -> (a -> Bool) -> T.Tree a -> T.Tree (Bool, a)
flagTree currOk p (T.Node n cs) =
  T.Node (r, n) (map (flagTree r p) cs)
  where
    r = if not currOk then False else p n


flagInfoTree
  :: Verbosity
  -> T.Tree Info
  -> T.Tree (Bool, Info)
flagInfoTree vb = flagTree True pdct
  where
    pdct = case vb of
      Silent -> const False
      Status -> const False
      Interesting -> \i -> iResult i == iInterestingIf i
      All -> const True

flagResultTree
  :: (PassVerbosity, FailVerbosity)
  -> T.Tree (RNode a)
  -> T.Tree (FRNode a)
flagResultTree (pv, fv) (T.Node n cs) =
  T.Node (b, n') (map (flagResultTree (pv, fv)) cs)
  where
    toBool v = case v of
      Silent -> False
      _ -> True
    b = case n of
      Right _ -> True
      Left (RInfo na psd rslts) -> toBool (if psd then pv else fv)
    n' = case n of
      Left (RInfo na psd rslts) -> Left fri
        where
          fri = FRInfo na psd rslts'
          rslts' = map (second (flagInfoTree (if b then pv else fv))) rslts
      o -> o

levelResultTree :: Int -> T.Tree (FRNode a) -> T.Tree (NumNode a)
levelResultTree lvl = numberLevels lvl f
  where
    f i (si, ei) = case ei of
      Left (FRInfo n p rs) -> NumInfo i si n p rs'
        where
          rs' = map (second (numberLevels 

filterTree :: (a -> Bool) -> T.Tree a -> Maybe (T.Tree a)
filterTree p (T.Node n cs) =
  if not . p $ n
  then Nothing
  else Just (T.Node n (mapMaybe (filterTree p) cs))

pruneResult :: Verbosity -> Result a -> Maybe (Result a)
pruneResult v tr = case v of
  Silent -> Nothing
  FailOnly -> pruneFailOnly tr
  Brief -> Just $ pruneBrief tr
  InterestingFails -> Just $ pruneInteresting tr
  AllFails -> Just $ pruneAllFails tr
  AllAll -> Just tr


pruneFailOnly :: Result a -> Maybe (Result a)
pruneFailOnly (Result t) = case filterTree pdRNode t of
  Nothing -> Nothing
  Just t' -> Just . Result . fmap rmResults $ t'
  where
    pdRNode r = case r of
      Right _ -> True
      Left ri -> not . srPassed $ ri
    rmResults r = case r of
      Right n -> Right n
      Left (RInfo n p _) -> Left (RInfo n p [])

pruneBrief :: Result a -> Result a
pruneBrief (Result (T.Node l cs)) = Result (T.Node l' cs')
  where
    l' = case l of
      Right n -> Right n
      Left (RInfo n p _) -> Left $ RInfo n p []
    cs' = map (unResult . pruneBrief . Result) cs

pruneInteresting :: Result a -> Result a
pruneInteresting (Result (T.Node l cs)) = Result (T.Node l' cs')
  where
    l' = case l of
      Right n -> Right n
      Left (RInfo n p rs) -> Left (RInfo n p rs')
        where
          rs' = mapMaybe f rs
          f (a, t) =
            if p
            then Nothing
            else case filterTree pdInfo t of
              Nothing -> Nothing
              Just t' -> Just (a, t')
          pdInfo i = iResult i == iInterestingIf i
    cs' = map (unResult . pruneInteresting . Result) cs

pruneAllFails :: Result a -> Result a
pruneAllFails (Result (T.Node l cs)) = Result (T.Node l' cs')
  where
    l' = case l of
      Right n -> Right n
      Left (RInfo n p rs) -> Left (RInfo n p rs')
        where
          rs' = if p then [] else rs
    cs' = map (unResult . pruneAllFails . Result) cs

resultList :: Result a -> [Passed]
resultList = mapMaybe toPassed . T.flatten . unResult
  where
    toPassed e = case e of
      Right _ -> Nothing
      Left (RInfo _ p _) -> Just p
