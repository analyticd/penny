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
  , SpaceCount
  , ColorToFile
  , ExS
  , PrednoteConf(..)
  , prednoteMain

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
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Tree as T
import qualified System.Console.MultiArg as MA
import qualified System.Console.Terminfo as TI
import qualified System.Exit as Exit
import qualified System.IO as IO

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


------------------------------------------------------------
-- CLI
------------------------------------------------------------

type ProgName = String
type BriefDesc = String
type MoreHelp = [String]
type ColorToFile = Bool

help
  :: ProgName
  -> BriefDesc
  -> MoreHelp
  -> Verbosity
  -> SpaceCount
  -> ColorToFile
  -> String
help pn bd ah v sc ctf = unlines $
  [ "usage: " ++ pn ++ "[options] ARGS"
  , ""
  , bd
  , "Options:"
  , ""
  , "--color-to-file no|yes"
  , "  If yes, use colors even when standard output is"
  , "  not a terminal. (default: " ++ dCtf ++ ")"
  , ""
  , "--verbosity, -v VERBOSITY"
  , "  Use the given level of verbosity. Choices:"
  , "    silent - show nothing at all"
  , "    fails - show only series that fail"
  , "    brief - show only whether each series succeeded or failed"
  , "    interesting - show interesting result from failed"
  , "      series; for successful series, show only that they"
  , "      succeeded"
  , "    allFails - show all result from failed series; for"
  , "      successful series, show only that they succeeded"
  , "    everything - show all results from all series"
  , "    (default: " ++ dVerb ++ ")"
  , ""
  , "--indentation, -i SPACES - indent each level by this many spaces"
  , "  (default: " ++ dSc ++ ")"
  , ""
  , "--help, -h - show help and exit"
  , ""
  ] ++ ah
  where
    dCtf = if ctf then "yes" else "no"
    dVerb = case v of
      Silent -> "silent"
      FailOnly -> "fails"
      Brief -> "brief"
      InterestingFails -> "interesting"
      AllFails -> "allFails"
      AllAll -> "everything"
    dSc = show sc

data Arg
  = AHelp
  | AVerbosity Verbosity
  | AColorToFile ColorToFile
  | AIndentation SpaceCount
  | APosArg String
  deriving Eq

optHelp :: MA.OptSpec Arg
optHelp = MA.OptSpec ["help"] "h" (MA.NoArg AHelp)

optVerbosity :: MA.OptSpec Arg
optVerbosity = MA.OptSpec ["verbosity"] "v" (MA.ChoiceArg ls)
  where
    ls = fmap (second AVerbosity) $
         [ ("silent", Silent)
         , ("fails", FailOnly)
         , ("brief", Brief)
         , ("interesting", InterestingFails)
         , ("allFails", AllFails)
         , ("everything", AllAll)
         ]

optColorToFile :: MA.OptSpec Arg
optColorToFile = MA.OptSpec ["color-to-file"] "" (MA.ChoiceArg ls)
  where
    ls = fmap (second AColorToFile) [ ("no", False), ("yes", True) ]

type ExS = Ex.Exceptional String
optIndentation :: MA.OptSpec (ExS Arg)
optIndentation = MA.OptSpec ["indentation"] "i" (MA.OneArg f)
  where
    f s =
      let err = Ex.throw $ "improper indentation argument: " ++ s
      in case reads s of
          (i, ""):[] ->
            if i >= 0 then Ex.Success (AIndentation i) else err
          _ -> err

data ParseResult
  = NeedsHelp
  | ParseErr String
  | Parsed Verbosity SpaceCount ColorToFile [String]

-- | When passed the defaults, return the values to use, as they might
-- have been affected by the command arguments, or return Nothing if
-- help is needed.
parseArgs
  :: Verbosity
  -> SpaceCount
  -> ColorToFile
  -> [String]
  -> ParseResult
parseArgs v sc ctf ss =
  let exLs = MA.simple MA.Intersperse opts (return . APosArg) ss
      opts = [ fmap return optHelp
             , fmap return optVerbosity
             , fmap return optColorToFile
             , optIndentation
             ]
  in case exLs of
      Ex.Exception e -> ParseErr . show $ e
      Ex.Success ls -> case sequence ls of
        Ex.Exception e -> ParseErr e
        Ex.Success ls' ->
          if AHelp `elem` ls'
          then NeedsHelp
          else Parsed (getVerbosity v ls') (getSpaceCount sc ls')
                      (getColorToFile ctf ls') (getPosArg ls')

getVerbosity :: Verbosity -> [Arg] -> Verbosity
getVerbosity v as = case mapMaybe f as of
  [] -> v
  xs -> last xs
  where f a = case a of { AVerbosity vb -> Just vb; _ -> Nothing }

getSpaceCount :: SpaceCount -> [Arg] -> SpaceCount
getSpaceCount sc as = case mapMaybe f as of
  [] -> sc
  xs -> last xs
  where f a = case a of { AIndentation i -> Just i; _ -> Nothing }

getColorToFile :: ColorToFile -> [Arg] -> ColorToFile
getColorToFile ctf as = case mapMaybe f as of
  [] -> ctf
  xs -> last xs
  where f a = case a of { AColorToFile i -> Just i; _ -> Nothing }

getPosArg :: [Arg] -> [String]
getPosArg = mapMaybe f
  where f a = case a of { APosArg s -> Just s; _ -> Nothing }

data PrednoteConf a = PrednoteConf
  { briefDescription :: String
  , moreHelp :: [String]
  , verbosity :: Verbosity
  , spaceCount :: SpaceCount
  , colorToFile :: ColorToFile
  , showSubject :: (a -> String)
  , groups :: [SeriesGroup a]
  , getSubjects :: ([String] -> IO (ExS [a]))
  }

exitWithCode :: [SeriesResult a] -> IO ()
exitWithCode srs =
  if and . concatMap getList $ srs
  then Exit.exitSuccess
  else Exit.exitFailure
  where
    getList res = case res of
      SingleResult (SRInfo _ p _) -> [p]
      SeveralResult (GroupResult _ rs) -> concatMap getList rs

applyParse
  :: ProgName
  -> PrednoteConf a
  -> [String]
  -> IO (Verbosity, SpaceCount, ColorToFile, [String])
applyParse pn c as = do
  case parseArgs (verbosity c) (spaceCount c) (colorToFile c) as of
    NeedsHelp -> do
      putStrLn (help pn (briefDescription c) (moreHelp c)
                (verbosity c) (spaceCount c) (colorToFile c))
      Exit.exitSuccess
    ParseErr e -> do
      putStrLn $ pn ++ ": could not parse command line: " ++ e
      Exit.exitFailure
    Parsed a1 a2 a3 a4 -> return (a1, a2, a3, a4)

prednoteMain :: PrednoteConf a -> IO ()
prednoteMain c = do
  pn <- MA.getProgName
  as <- MA.getArgs
  (vbsty, sc, ctf, posargs) <- applyParse pn c as
  isTerm <- IO.hIsTerminalDevice IO.stdout
  ti <- if isTerm || ctf
          then TI.setupTermFromEnv
          else TI.setupTerm "dumb"
  exSubs <- getSubjects c posargs
  subs <- case exSubs of
    Ex.Exception s -> do
      putStrLn $ pn ++ ": error processing positional arguments: " ++ s
      Exit.exitFailure
    Ex.Success ss -> return ss
  let srs = map (runSeries subs) . groups $ c
  mapM_ (showSeries ti (showSubject c) sc vbsty) srs
  exitWithCode srs
