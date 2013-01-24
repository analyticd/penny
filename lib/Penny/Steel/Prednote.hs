-- | Prednote - annotated predicates
--
-- Prednote allows you to annotate a predicate with a name. You can
-- then combine those predicates to form a new predicate. When using
-- the predicate to test particular subjects, you will get a tree that
-- can show you all successes and failures.

module Penny.Steel.Prednote
  ( -- * Pdct
    Pdct(..)
  , PdctOutput
  , Name

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
  , seeTrue
  , seeFalse

    -- * Tests
    -- * Test types
  , Pass
  , Interesting
  , FirstPdctResult(..)
  , Test
  , TestResult(..)
  , GroupResult(..)
  , Verbosity(..)
  , PassVerbosity
  , FailVerbosity
  , SpaceCount
  , Result

  -- * Creating tests
  , seriesAtLeastN
  , eachSubjectMustBeTrue
  , processTrueSubjects
  , group

    -- ** Running test series
  , runTest
  , showResults

  ) where

import Control.Arrow ((&&&))
import Control.Applicative ((<*>), pure)
import qualified Data.Tree as T
import qualified System.Console.Terminfo as TI
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import Data.Text (Text, pack)

------------------------------------------------------------
-- Types
------------------------------------------------------------

--
-- INPUT Predicates
--

type Name = Text
type Pass = Bool
type Interesting = Bool

-- | The result that the predicates return.
type PdctOutput = (Pass, Name, Interesting)

-- | A Pdct is the building block of a test. The top node of the tree
-- holds the result of the predicate. Child nodes can hold anything
-- you want but typically they will hold the results of other
-- predicates that were used to compute the result.
newtype Pdct a = Pdct { unPdct :: a -> T.Tree PdctOutput }

--
-- INPUT Tests
--
type TestOutput a = (Pass, Name, [(a, T.Tree PdctOutput)])
type TestOrGroup a = Either ([a] -> TestOutput a) Name

newtype Test a = Test { unTest :: T.Tree (TestOrGroup a) }

--
-- OUTPUT Predicates
--

type Level = Int
type Shown = Bool
data PdctResult = PdctResult
  { _pdctPass :: Pass
  , _pdctName :: Name
  , _pdctInteresting :: Interesting
  , _pdctLevel :: Level
  , _pdctShown :: Shown
  } deriving (Eq, Show)

--
-- OUTPUT Tests
--

data FirstPdctResult a = FirstPdctResult
  { firstPass :: Pass
  , firstName :: Name
  , firstInteresting :: Interesting
  , firstLevel :: Level
  , firstShown :: Shown
  , firstSubject :: a
  } deriving (Eq, Show)

data TestResult a = TestResult
  { testPass :: Pass
  , testName :: Name
  , testResults :: [(FirstPdctResult a, [T.Tree PdctResult])]
  , testLevel :: Level
  , testShown :: Shown
  } deriving (Eq, Show)

data GroupResult = GroupResult
  { groupName :: Name
  , groupLevel :: Level
  , groupShown :: Shown
  } deriving (Eq, Show)

newtype Result a =
  Result { unResult :: T.Tree (Either (TestResult a) GroupResult) }
  deriving (Eq, Show)

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
pdct :: Name -> (a -> Bool) -> Pdct a
pdct d p = Pdct fn
  where
    fn pf = T.Node n []
      where
        psd = p pf
        n = (psd, d, not psd)

-- | Always returns True.
true :: Pdct a
true = Pdct fn
  where
    fn _ = T.Node n []
      where
        n = (True, pack "always True", False)

-- | Always returns False.
false :: Pdct a
false = Pdct fn
  where
    fn _ = T.Node n []
      where
        n = (False, pack "always False", False)

------------------------------------------------------------
-- Combinators
------------------------------------------------------------

-- | Succeeds only when the given test fails.
expectFail :: Pdct a -> Pdct a
expectFail (Pdct t) = Pdct fn
  where
    d = pack "boolean not"
    fn s = T.Node n [c]
      where
        c@(T.Node (rslt, _, _) _) = t s
        n = ((not rslt), d, rslt)

-- | Renames a predicate.
rename :: Name -> Pdct a -> Pdct a
rename d (Pdct t) = Pdct fn
  where
    fn s = T.Node l' cs
      where
        T.Node (p, _, i) cs = t s
        l' = (p, d, i)

-- | Operator version of 'rename'.
(<?>) :: Name -> Pdct a -> Pdct a
(<?>) = rename
infix 0 <?>

-- | Disjunction: succeeds unless all argument tests fail.  Fails if
-- test list is empty.
disjoin :: [Pdct a] -> Pdct a
disjoin ls = Pdct fn
  where
    fn pf = T.Node (rslt, pack "disjunction", not rslt) fs
      where
        fs = map unPdct ls <*> pure pf
        rslt = any ((\(p, _, _) -> p) . T.rootLabel) fs

-- | Operator version of 'disjoin', uses a generic name.
(.||.) :: Pdct a -> Pdct a -> Pdct a
(.||.) x y = disjoin [x, y]

infixr 2 .||.

-- | Conjunction: succeeds if all are Good.  Succeeds if test list is
-- empty.
conjoin :: [Pdct a] -> Pdct a
conjoin ls = Pdct fn
  where
    fn pf = T.Node (rslt, pack "conjunction", not rslt) fs
      where
        fs = map unPdct ls <*> pure pf
        rslt = all ((\(p, _, _) -> p) . T.rootLabel) fs


-- | Operator version of conjoin, uses a generic description.
(.&&.) :: Pdct a -> Pdct a -> Pdct a
(.&&.) x y = conjoin [x, y]

infixr 3 .&&.

-- | Modifies a test so that True results are interesting.
seeTrue :: Pdct a -> Pdct a
seeTrue (Pdct f) = Pdct f'
  where
    f' x =
      let T.Node (p, n, _) cs = f x
      in T.Node (p, n, p) cs

-- | Modifies a test so that False results are interesting.
seeFalse :: Pdct a -> Pdct a
seeFalse (Pdct f) = Pdct f'
  where
    f' x =
      let T.Node (p, n, _) cs = f x
      in T.Node (p, n, not p) cs

------------------------------------------------------------
-- Series
------------------------------------------------------------

-- | True if the list is at least n elements long. Less strict than
-- 'length'.
atLeast :: Int -> [a] -> Bool
atLeast x = go 0
  where
    go i [] = i >= x
    go i (_:as) =
      if i >= x
      then True
      else let i' = i + 1 in i' `seq` go i' as

-- | Passes if at least n subjects are True.
seriesAtLeastN
  :: Name
  -> Int
  -> Pdct a
  -> Test a
seriesAtLeastN name i (Pdct t) = Test $ T.Node (Left fn) []
  where
    fn pfs = (atl, name, pairs)
      where
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
        atl = atLeast i
              . filter id
              . map ((\(p, _, _) -> p) . T.rootLabel)
              $ rslts

{-
-- | Every subject is run through the test. Each subject must return
-- True; otherwise the test fails.
eachSubjectMustBeTrue
  :: Name
  -> Pdct a
  -> Test a
eachSubjectMustBeTrue n (Pdct t) = Test $ T.Node (Left fn) []
  where
    fn pfs = (passed, n, pairs)
      where
        passed = all ((\(p, _, _) -> p) . T.rootLabel) rslts
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
-}
-- | Every subject is run through the test. Each subject must return
-- True; otherwise the test fails.
eachSubjectMustBeTrue
  :: Name
  -> Pdct a
  -> Test a
eachSubjectMustBeTrue n (Pdct t) = Test $ T.Node (Left fn) []
  where
    fn pfs =
      let mkOut = (zip pfs)
                  &&& (all ((\(p, _, _) -> p) . T.rootLabel))
          toTup (pairs, passed) = (passed, n, pairs)
          mkRslts = (pure t <*>)
      in toTup . mkOut . mkRslts $ pfs

-- | Every subject is run through the test. Subjects that return True
-- are then fed to the given function. The result of the function is
-- the result of the test.
processTrueSubjects
  :: Name
  -> ([a] -> Bool)
  -> Pdct a
  -> Test a
processTrueSubjects name fp (Pdct t) = Test $ T.Node (Left fn) []
  where
    fn pfs = (r, name, pairs)
      where
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
        r = fp . map fst
            . filter ((\(p, _, _) -> p) . T.rootLabel . snd) $ pairs

group :: Name -> [Test a] -> Test a
group n = Test . T.Node (Right n) . map unTest

runTest
  :: PassVerbosity
  -> FailVerbosity
  -> [a]
  -> CurrLevel
  -> Test a
  -> Result a
runTest pv fv as l = Result . runTest' pv fv as l . unTest

type CurrLevel = Int

runTest'
  :: PassVerbosity
  -> FailVerbosity
  -> [a]
  -> CurrLevel
  -> T.Tree (TestOrGroup a)
  -> T.Tree (Either (TestResult a) GroupResult)
runTest' pv fv as l (T.Node ei cs) = T.Node rn cs'
  where
    cs' = let l' = l + 1 in l' `seq` map (runTest' pv fv as l') cs
    rn = case ei of
      Right g -> Right (GroupResult g l s)
        where
          s = case (pv, fv) of
            (Silent, Silent) -> False
            _ -> True
      Left fn -> Left (runFn pv fv as l fn)

runFn
  :: PassVerbosity
  -> FailVerbosity
  -> [a]
  -> CurrLevel
  -> ([a] -> TestOutput a)
  -> TestResult a
runFn pv fv as l fn = TestResult psd n rs l showThis
  where
    (psd, n, lst) = fn as
    v = if psd then pv else fv
    (showThis, showTree) = case v of
      Silent -> (False, False)
      Status -> (True, False)
      Interesting -> (True, True)
      All -> (True, True)
    rs = let l' = l + 1
         in l' `seq` map (convertPdctOutput v showTree l') lst

type ParentShown = Bool

convertPdctOutput
  :: Verbosity
  -> ParentShown
  -> CurrLevel
  -> (a, T.Tree PdctOutput)
  -> (FirstPdctResult a, [T.Tree PdctResult])
convertPdctOutput v pw l (a, (T.Node n ls)) = (r1, ls')
  where
    (r1p, r1n, r1i) = n
    r1 = FirstPdctResult r1p r1n r1i l swn a
    swn = if not pw then False else case v of
      Silent -> False
      Status -> False
      Interesting -> r1i
      _ -> True
    ls' = let l' = l + 1
          in l' `seq` map (toPdctResult v swn l') ls

toPdctResult
  :: Verbosity
  -> ParentShown
  -> CurrLevel
  -> T.Tree PdctOutput
  -> T.Tree PdctResult
toPdctResult v pw l (T.Node n ls) = T.Node n' ls'
  where
    (p, s, i) = n
    n' = PdctResult p s i l swn
    swn = if not pw then False else case v of
      Silent -> False
      Status -> False
      Interesting -> i
      All -> True
    ls' = let l' = l + 1
          in l' `seq` map (toPdctResult v swn l') ls

------------------------------------------------------------
-- Showing
------------------------------------------------------------

type Success = Bool

showResults
  :: TI.Terminal
  -> SpaceCount
  -> (a -> Text)
  -> Result a
  -> IO Success
showResults ti sc swr
  = fmap and
  . mapM (showResult ti sc swr)
  . T.flatten
  . unResult

showResult
  :: TI.Terminal
  -> SpaceCount
  -> (a -> Text)
  -> Either (TestResult a) GroupResult
  -> IO Success
showResult t sc swr =
  either (showTestResult t sc swr) (fmap (const True) . showGroupResult sc)

showTestResult
  :: TI.Terminal
  -> SpaceCount
  -> (a -> Text)
  -> TestResult a
  -> IO Success
showTestResult ti sc swr (TestResult p n rs l s) =
  if not s
  then return p
  else do
    let ind = X.replicate (sc * l) (X.singleton ' ')
    TIO.putStr ind
    printOkFail ti p
    putStr " "
    TIO.putStrLn n
    mapM_ (showPdctResultPair ti sc swr) rs
    return p

showPdctResultPair
  :: TI.Terminal
  -> SpaceCount
  -> (a -> Text)
  -> (FirstPdctResult a, [T.Tree PdctResult])
  -> IO ()
showPdctResultPair ti sc swr (r1, ls)
  | not (firstShown r1) = return ()
  | otherwise = do
      let ind = X.replicate (sc * l) (X.singleton ' ')
          FirstPdctResult p n _ l _ s = r1
      TIO.putStr ind
      printTrueFalse ti p
      putStr " "
      TIO.putStr n
      putStr " "
      TIO.putStrLn . swr $ s
      mapM_ (showPdctResult ti sc) . concatMap T.flatten $ ls

showPdctResult :: TI.Terminal -> SpaceCount -> PdctResult -> IO ()
showPdctResult ti sc (PdctResult p n _ l s)
  | not s = return ()
  | otherwise = do
      let ind = X.replicate (sc * l) (X.singleton ' ')
      TIO.putStr ind
      printTrueFalse ti p
      putStr " "
      TIO.putStrLn n

showGroupResult :: SpaceCount -> GroupResult -> IO ()
showGroupResult sc (GroupResult n l s)
  | not s = return ()
  | otherwise = do
      let ind = X.replicate (sc * l) (X.singleton ' ')
      TIO.putStr ind
      TIO.putStrLn n

--
-- Colors
--

printInColor :: TI.Terminal -> TI.Color -> Text -> IO ()
printInColor t c s =
  case TI.getCapability t TI.setForegroundColor of
    Nothing -> TIO.putStr s
    Just cap -> do
      TI.runTermOutput t (cap c)
      TIO.putStr s
      case TI.getCapability t TI.restoreDefaultColors of
        Nothing -> return ()
        Just r -> TI.runTermOutput t r

printTrueFalse :: TI.Terminal -> Pass -> IO ()
printTrueFalse t r = do
  putStr "["
  if r
    then do
      printInColor t TI.Green (pack "TRUE")
      putStr "] "
    else do
      printInColor t TI.Red (pack "FALSE")
      putStr "]"

printOkFail :: TI.Terminal -> Pass -> IO ()
printOkFail t p = do
  putStr "["
  if p
    then printInColor t TI.Green (pack " OK ")
    else printInColor t TI.Red (pack "FAIL")
  putStr "]"

