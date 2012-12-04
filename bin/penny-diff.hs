module Main where

import qualified Control.Exception as CEx
import Control.Monad (when)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe (fromJust)
import Data.List (deleteFirstsBy)
import qualified System.Console.MultiArg as M
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Copper as C
import qualified Penny.Copper.Render as CR
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified System.Exit as E
import qualified System.IO as IO

groupingSpecs :: CR.GroupSpecs
groupingSpecs = CR.GroupSpecs CR.NoGrouping CR.NoGrouping

main :: IO ()
main = runPennyDiff groupingSpecs

help :: String
help = unlines
  [ "usage: penny-diff [-12] FILE1 FILE2"
  , "Shows items that exist in FILE1 but not in FILE2,"
  , "as well as items that exist in FILE2 but not in FILE1."
  , "Options:"
  , "-1 Show only items that exist in FILE1 but not in FILE2"
  , "-2 Show only items that exist in FILE2 but not in FILE1"
  ]

data Args = ArgFile File | Filename String | Help
  deriving (Eq, Show)

data DiffsToShow = File1Only | File2Only | BothFiles

optFile1 :: M.OptSpec Args
optFile1 = M.OptSpec [] "1" (M.NoArg (ArgFile File1))

optFile2 :: M.OptSpec Args
optFile2 = M.OptSpec [] "2" (M.NoArg (ArgFile File2))

data File = File1 | File2
  deriving (Eq, Show)

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
  C.PricePoint p -> Just (Price p)
  C.IComment c -> Just (Comment c)
  _ -> Nothing


showLineNum :: File -> Int -> X.Text
showLineNum f i = X.pack ("\n" ++ arrow ++ " " ++ show i ++ "\n")
  where
    arrow = case f of
      File1 -> "<=="
      File2 -> "==>"


-- | Renders a transaction, along with a line showing what file it
-- came from and its line number. If there is a TransactionMemo, shows
-- the line number for the top line for that; otherwise, shows the
-- line number for the TopLine.
renderTransaction
  :: CR.GroupSpecs
  -> File
  -> L.Transaction
  -> Maybe X.Text
renderTransaction gs f t = fmap addHeader $ CR.transaction gs t
  where
    (L.Family tl _ _ _) = L.unTransaction t
    lin = case L.tMemo tl of
      Nothing -> L.unTopLineLine . fromJust
                 . L.topLineLine . L.tMeta $ tl
      Just _ -> L.unTopMemoLine . fromJust
                . L.topMemoLine . L.tMeta $ tl
    addHeader x = (showLineNum f lin) `X.append` x

renderPrice :: CR.GroupSpecs -> File -> L.PricePoint -> Maybe X.Text
renderPrice gs f p = fmap addHeader $ CR.price gs p
  where
    lin = L.unPriceLine . fromJust . L.priceLine . L.ppMeta $ p
    addHeader x = (showLineNum f lin) `X.append` x

renderNonBlankItem :: CR.GroupSpecs -> File -> NonBlankItem -> Maybe X.Text
renderNonBlankItem gs f n = case n of
  Transaction t -> renderTransaction gs f t
  Price p -> renderPrice gs f p
  Comment c -> CR.comment c

runPennyDiff :: CR.GroupSpecs -> IO ()
runPennyDiff co = do
  (f1, f2, dts) <- parseCommandLine
  l1 <- parseFile f1
  l2 <- parseFile f2
  let (r1, r2) = doDiffs l1 l2
  showDiffs co dts (r1, r2)
  case (r1, r2) of
    ([], []) -> E.exitSuccess
    _ -> E.exitWith (E.ExitFailure 1)

showDiffs
  :: CR.GroupSpecs
  -> DiffsToShow
  -> ([NonBlankItem], [NonBlankItem])
  -> IO ()
showDiffs co dts (l1, l2) =
  case dts of
    File1Only -> showFile1
    File2Only -> showFile2
    BothFiles -> showFile1 >> showFile2
  where
    showFile1 = showNonBlankItems co File1 l1
    showFile2 = showNonBlankItems co File2 l2

failure :: String -> IO a
failure s = IO.hPutStrLn IO.stderr s
  >> E.exitWith (E.ExitFailure 2)

showNonBlankItems
  :: CR.GroupSpecs
  -> File
  -> [NonBlankItem]
  -> IO ()
showNonBlankItems o f ls =
  mapM_ (showNonBlankItem o f) ls

showNonBlankItem :: CR.GroupSpecs -> File -> NonBlankItem -> IO ()
showNonBlankItem co f nbi = maybe e TIO.putStr
  (renderNonBlankItem co f nbi)
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
    mkNbList = mapMaybe toNonBlankItem . C.unLedger
    (nb1, nb2) = (mkNbList l1, mkNbList l2)
    df = deleteFirstsBy clonedNonBlankItem
    (r1, r2) = (nb1 `df` nb2, nb2 `df` nb1)

parseFile :: String -> IO C.Ledger
parseFile s = do
  eiTxt <- CEx.try $ TIO.readFile s
  txt <- case eiTxt of
    Left e -> failure (show (e :: IOError))
    Right g -> return g
  let fn = L.Filename . X.pack $ s
      c = C.FileContents txt
      parsed = C.parse [(fn, c)]
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
  ls <- case parsed of
    Ex.Exception e ->
      let err = "penny-diff: could not parse command line: "
            ++ show e
      in failure err
    Ex.Success g -> return g
  when (any (== Help) ls) (putStrLn help >> E.exitSuccess)
  let toFilename a = case a of
        Filename s -> Just s
        _ -> Nothing
  (fn1, fn2) <- case mapMaybe toFilename ls of
    x:y:[] -> return (x, y)
    _ -> failure "penny-diff: error: you must supply two filenames."
  let getDiffs
        | ((ArgFile File1) `elem` ls)
          && ((ArgFile File2) `elem` ls) = BothFiles
        | ((ArgFile File1) `elem` ls) = File1Only
        | ((ArgFile File2) `elem` ls) = File2Only
        | otherwise = BothFiles
  return (fn1, fn2, getDiffs)
