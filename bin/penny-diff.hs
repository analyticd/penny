module Main where

import qualified Control.Exception as CEx
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (deleteFirstsBy)
import qualified System.Console.MultiArg as M
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Copper as C
import qualified Penny.Copper.Transaction as CT
import qualified Penny.Copper.Price as CP
import qualified Penny.Copper.Comments as CC
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified System.Exit as E
import qualified System.IO as IO

defaultCopperOptions :: CopperOptions
defaultCopperOptions = CopperOptions C.utcDefault
  (C.GroupAll, C.GroupAll) C.periodComma


main :: IO ()
main = runPennyDiff defaultCopperOptions

data CopperOptions = CopperOptions
  { defaultTimeZone :: C.DefaultTimeZone
  , groupingSpecs :: (C.GroupingSpec, C.GroupingSpec)
  , radGroup :: C.RadGroup
  } deriving Show

help :: String
help = unlines
  [ "usage: penny-diff [-12] FILE1 FILE2"
  , "Shows items that exist in FILE1 but not in FILE2,"
  , "as well as items that exist in FILE2 but not in FILE1."
  , "Options:"
  , "-1 Show only items that exist in FILE1 but not in FILE2"
  , "-2 Show only items that exist in FILE2 but not in FILE1"
  ]

data Args = File1 | File2 | Filename String
  deriving (Eq, Show)

data DiffsToShow = File1Only | File2Only | BothFiles

optFile1 :: M.OptSpec Args
optFile1 = M.OptSpec [] "1" (M.NoArg File1)

optFile2 :: M.OptSpec Args
optFile2 = M.OptSpec [] "2" (M.NoArg File2)

-- | All possible items, but excluding blank lines.
data NonBlankItem
  = Transaction L.Transaction
  | Price L.PricePoint
  | Comment C.Comment
  deriving (Eq, Show)

clonedNonBlankItem :: NonBlankItem -> NonBlankItem -> Bool
clonedNonBlankItem nb1 nb2 = case (nb1, nb2) of
  (Transaction t1, Transaction t2) -> P.clonedTransactions t1 t2
  (Price p1, Price p2) -> p1 == p2
  (Comment c1, Comment c2) -> c1 == c2
  _ -> False

toNonBlankItem :: C.Item -> Maybe NonBlankItem
toNonBlankItem i = case i of
  C.Transaction t -> Just (Transaction t)
  C.Price p -> Just (Price p)
  C.CommentItem c -> Just (Comment c)
  _ -> Nothing

renderNonBlankItem :: CopperOptions -> NonBlankItem -> Maybe X.Text
renderNonBlankItem (CopperOptions dtz gs rg) n = case n of
  Transaction t -> CT.render dtz gs rg t
  Price p -> CP.render dtz gs rg p
  Comment c -> CC.render c

runPennyDiff :: CopperOptions -> IO ()
runPennyDiff co = do
  (f1, f2, dts) <- parseCommandLine
  l1 <- parseFile co f1
  l2 <- parseFile co f2
  let (r1, r2) = doDiffs l1 l2
  showDiffs co dts (f1, f2) (r1, r2)
  case (r1, r2) of
    ([], []) -> E.exitSuccess
    _ -> E.exitWith (E.ExitFailure 1)

showDiffs
  :: CopperOptions
  -> DiffsToShow
  -> (String, String)
  -> ([NonBlankItem], [NonBlankItem])
  -> IO ()
showDiffs co dts (n1, n2) (l1, l2) =
  case dts of
    File1Only -> showFile1
    File2Only -> showFile2
    BothFiles -> showFile1 >> showFile2
  where
    showFile1 = showNonBlankItems
      ("Items in " ++ n1 ++ " not in " ++ n2) co l1
    showFile2 = showNonBlankItems
      ("Items in " ++ n2 ++ " not in " ++ n1) co l2

failure :: String -> IO a
failure s = IO.hPutStrLn IO.stderr s
  >> E.exitWith (E.ExitFailure 2)

showNonBlankItems
  :: String
  -- ^ Description of items to show
  -> CopperOptions
  -> [NonBlankItem]
  -> IO ()
showNonBlankItems s o ls =
  putStrLn s
  >> mapM_ (showNonBlankItem o) ls

showNonBlankItem :: CopperOptions -> NonBlankItem -> IO ()
showNonBlankItem co nbi = maybe e TIO.putStr
  (renderNonBlankItem co nbi)
  where
    e = failure $ "could not render item: " ++ show nbi


-- | Returns a pair p, where fst p is the items that appear in file1
-- but not in file2, and snd p is items that appear in file2 but not
-- in file1.
doDiffs
  :: C.Ledger
  -> C.Ledger
  -> ([NonBlankItem], [NonBlankItem])
doDiffs l1 l2 = (r1, r2)
  where
    mkNbList = mapMaybe (toNonBlankItem . snd) . C.unLedger
    (nb1, nb2) = (mkNbList l1, mkNbList l2)
    df = deleteFirstsBy clonedNonBlankItem
    (r1, r2) = (nb1 `df` nb2, nb2 `df` nb1)

parseFile :: CopperOptions -> String -> IO C.Ledger
parseFile (CopperOptions dtz _ rg) s = do
  eiTxt <- CEx.try $ TIO.readFile s
  txt <- case eiTxt of
    Left e -> failure (show (e :: IOError))
    Right g -> return g
  let fn = L.Filename . X.pack $ s
      c = C.FileContents txt
      parsed = C.parse dtz rg [(fn, c)]
  case parsed of
    Ex.Exception e -> failure (show e)
    Ex.Success g -> return g


-- | Returns a tuple with the first filename, the second filename, and
-- an indication of which differences to show.
parseCommandLine :: IO (String, String, DiffsToShow)
parseCommandLine = do
  as <- M.getArgs
  let parsed = M.parse
                M.Intersperse [optFile1, optFile2] Filename as
  case parsed of
    Ex.Exception e ->
      let err = "penny-diff: could not parse command line: "
            ++ show e
      in failure err
    Ex.Success g ->
      let toFilename a = case a of
            Filename s -> Just s
            _ -> Nothing
      in case mapMaybe toFilename g of
          fn1:fn2:[] ->
            let getDiffs
                  | (File1 `elem` g) && (File2 `elem` g) = BothFiles
                  | (File1 `elem` g) = File1Only
                  | (File2 `elem` g) = File2Only
                  | otherwise = BothFiles
            in return (fn1, fn2, getDiffs)
          _ ->
            let err = "penny-diff: error: you must supply "
                  ++ "two filenames."
            in failure err

