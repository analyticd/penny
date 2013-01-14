module Penny.Wheat
  ( Pdct
  , payee
  , number
  , flag
  , postingMemo
  , transactionMemo
  , dateTime
  , CompQty(..)
  , qty
  , drCr
  , commodity
  , account
  , accountLevel
  , accountAny
  , tag
  ) where

import Control.Arrow (second)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (mapMaybe)
import qualified Penny.Steel.Prednote as N
import Penny.Steel.Prednote (pdct)
import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Text.Matchers as M
import qualified System.Console.MultiArg as MA
import qualified System.Exit as Exit

type Pdct = N.Pdct L.PostFam

-- * Pattern matching fields

descItem :: String -> M.Matcher -> String
descItem s m = "field: " ++ s
               ++ " matcher description: "
               ++ (X.unpack $ M.matchDesc m)

payee :: M.Matcher -> Pdct
payee p = pdct (descItem "payee" p) (P.payee (M.match p))


number :: M.Matcher -> Pdct
number p = pdct (descItem "number" p) (P.number (M.match p))

flag :: M.Matcher -> Pdct
flag p = pdct (descItem "flag" p) (P.flag (M.match p))

postingMemo :: M.Matcher -> Pdct
postingMemo p = pdct (descItem "posting memo" p)
                (P.postingMemo (M.match p))

transactionMemo :: M.Matcher -> Pdct
transactionMemo p = pdct (descItem "transaction memo" p)
                (P.transactionMemo (M.match p))

-- * UTC times

dateTime :: M.CompUTC -> T.UTCTime -> Pdct
dateTime c t = pdct ("posting " ++ M.descUTC c t) p
  where
    p = (`cmp` t) . L.toUTC . Q.dateTime
    cmp = M.compUTCtoCmp c

-- * Quantities

data CompQty
  = QGT
  | QGTEQ
  | QEQ
  | QLTEQ
  | QLT

descQty :: CompQty -> L.Qty -> String
descQty c q = "quantity of posting is " ++ co ++ " " ++ show q
  where
    co = case c of
      QGT -> "greater than"
      QGTEQ -> "greater than or equal to"
      QEQ -> "equal to"
      QLTEQ -> "less than or equal to"
      QLT -> "less than"

qty :: CompQty -> L.Qty -> Pdct
qty c q = pdct (descQty c q) p
  where
    p = (`cmp` q) . Q.qty
    cmp = case c of
      QGT -> (>)
      QGTEQ -> (>=)
      QEQ -> (==)
      QLTEQ -> (<=)
      QLT -> (<)

-- * DrCr

drCr :: L.DrCr -> Pdct
drCr dc = pdct ("posting is a " ++ desc) p
  where
    desc = case dc of
      L.Debit -> "debit"
      L.Credit -> "credit"
    p = (== dc) . Q.drCr

-- * Commodity

commodity :: M.Matcher -> Pdct
commodity p = pdct (descItem "commodity" p) (P.commodity (M.match p))

-- * Account name

account :: M.Matcher -> Pdct
account p = pdct (descItem "full account name" p)
            (P.account (X.singleton ':') (M.match p))

accountLevel :: Int -> M.Matcher -> Pdct
accountLevel i p = pdct (descItem ("sub-account " ++ show i) p)
                   (P.accountLevel i (M.match p))

accountAny :: M.Matcher -> Pdct
accountAny p = pdct (descItem "any sub-account" p)
               (P.accountAny (M.match p))

-- * Tags

tag :: M.Matcher -> Pdct
tag p = pdct (descItem "any tag" p) (P.tag (M.match p))

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
  -> N.Verbosity
  -> N.SpaceCount
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
      N.Silent -> "silent"
      N.FailOnly -> "fails"
      N.Brief -> "brief"
      N.InterestingFails -> "interesting"
      N.AllFails -> "allFails"
      N.AllAll -> "everything"
    dSc = show sc

data Arg
  = AHelp
  | AVerbosity N.Verbosity
  | AColorToFile ColorToFile
  | AIndentation N.SpaceCount
  | APosArg String
  deriving Eq

optHelp :: MA.OptSpec Arg
optHelp = MA.OptSpec ["help"] "h" (MA.NoArg AHelp)

optVerbosity :: MA.OptSpec Arg
optVerbosity = MA.OptSpec ["verbosity"] "v" (MA.ChoiceArg ls)
  where
    ls = fmap (second AVerbosity) $
         [ ("silent", N.Silent)
         , ("fails", N.FailOnly)
         , ("brief", N.Brief)
         , ("interesting", N.InterestingFails)
         , ("allFails", N.AllFails)
         , ("everything", N.AllAll)
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
  | Parsed N.Verbosity N.SpaceCount ColorToFile [String]

-- | When passed the defaults, return the values to use, as they might
-- have been affected by the command arguments, or return Nothing if
-- help is needed.
parseArgs
  :: N.Verbosity
  -> N.SpaceCount
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

getVerbosity :: N.Verbosity -> [Arg] -> N.Verbosity
getVerbosity v as = case mapMaybe f as of
  [] -> v
  xs -> last xs
  where f a = case a of { AVerbosity vb -> Just vb; _ -> Nothing }

getSpaceCount :: N.SpaceCount -> [Arg] -> N.SpaceCount
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

data WheatConf = WheatConf
  { briefDescription :: String
  , moreHelp :: [String]
  , verbosity :: N.Verbosity
  , spaceCount :: N.SpaceCount
  , colorToFile :: ColorToFile
  , groups :: [N.SeriesGroup L.PostFam]
  }

applyParse
  :: ProgName
  -> WheatConf
  -> [String]
  -> IO (N.Verbosity, N.SpaceCount, ColorToFile, [String])
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

{-
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
-}
