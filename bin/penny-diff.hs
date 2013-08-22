module Main where

import Control.Arrow (first, second)
import Data.Maybe (fromJust)
import Data.List (deleteFirstsBy)
import qualified System.Console.MultiArg as M
import qualified Penny as P
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import Penny.Lincoln ((==~))
import qualified Penny.Copper as C
import qualified Penny.Copper.Render as CR
import qualified Penny.Steel.Sums as S
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified System.Exit as E
import qualified System.IO as IO

import qualified Paths_penny_bin as PPB

main :: IO ()
main = runPennyDiff

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " [-12] FILE1 FILE2"
  , "Shows items that exist in FILE1 but not in FILE2,"
  , "as well as items that exist in FILE2 but not in FILE1."
  , "Options:"
  , "-1 Show only items that exist in FILE1 but not in FILE2"
  , "-2 Show only items that exist in FILE2 but not in FILE1"
  , ""
  , "--help, -h - show this help and exit"
  , "--version Show version and exit"
  ]

data Args = ArgFile File | Filename String
  deriving (Eq, Show)

data DiffsToShow = File1Only | File2Only | BothFiles

optFile1 :: M.OptSpec Args
optFile1 = M.OptSpec [] "1" (M.NoArg (ArgFile File1))

optFile2 :: M.OptSpec Args
optFile2 = M.OptSpec [] "2" (M.NoArg (ArgFile File2))

allOpts :: [M.OptSpec Args]
allOpts = [ optFile1 , optFile2 ]

data File = File1 | File2
  deriving (Eq, Show)

-- | All possible items, but excluding blank lines.
type NonBlankItem =
  S.S3 L.Transaction L.PricePoint C.Comment

removeMeta
  :: L.Transaction
  -> (L.TopLineCore, L.Ents L.PostingCore)
removeMeta
  = first L.tlCore
  . second (fmap L.pdCore)
  . L.unTransaction

clonedNonBlankItem :: NonBlankItem -> NonBlankItem -> Bool
clonedNonBlankItem nb1 nb2 = case (nb1, nb2) of
  (S.S3a t1, S.S3a t2) -> removeMeta t1 ==~ removeMeta t2
  (S.S3b p1, S.S3b p2) -> p1 ==~ p2
  (S.S3c c1, S.S3c c2) -> c1 == c2
  _ -> False

toNonBlankItem :: C.LedgerItem -> Maybe NonBlankItem
toNonBlankItem = S.caseS4 (Just . S.S3a) (Just . S.S3b) (Just . S.S3c)
                          (const Nothing)

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
  :: File
  -> L.Transaction
  -> Maybe X.Text
renderTransaction gs f t = fmap addHeader $ CR.transaction (noMeta t)
  where
    lin = case L.tMemo . L.tlCore . fst . L.unTransaction $ t of
      Nothing -> L.unTopLineLine . L.tTopLineLine . fromJust
                 . L.tlFileMeta . fst . L.unTransaction $ t
      Just _ -> L.unTopMemoLine . fromJust . L.tTopMemoLine . fromJust
                . L.tlFileMeta . fst . L.unTransaction $ t
    addHeader x = (showLineNum f lin) `X.append` x
    noMeta txn = let (tl, es) = L.unTransaction txn
                 in (L.tlCore tl, fmap L.pdCore es)

renderPrice :: CR.GroupSpecs -> File -> L.PricePoint -> Maybe X.Text
renderPrice gs f p = fmap addHeader $ CR.price gs p
  where
    lin = L.unPriceLine . fromJust . L.priceLine $ p
    addHeader x = (showLineNum f lin) `X.append` x

renderNonBlankItem
  :: (L.Amount L.Qty -> X.Text)
  -> File
  -> NonBlankItem
  -> Maybe X.Text
renderNonBlankItem gs f =
  S.caseS3 (renderTransaction gs f) (renderPrice gs f) CR.comment

runPennyDiff :: IO ()
runPennyDiff = do
  (f1, f2, dts) <- parseCommandLine
  l1 <- C.open [f1]
  l2 <- C.open [f2]
  let (r1, r2) = doDiffs l1 l2
  showDiffs co dts (r1, r2)
  case (r1, r2) of
    ([], []) -> E.exitSuccess
    _ -> E.exitWith (E.ExitFailure 1)

showDiffs
  :: (L.Amount L.Qty -> X.Text)
  -> DiffsToShow
  -> ([NonBlankItem], [NonBlankItem])
  -> IO ()
showDiffs fmt dts (l1, l2) =
  case dts of
    File1Only -> showFile1
    File2Only -> showFile2
    BothFiles -> showFile1 >> showFile2
  where
    showFile1 = showNonBlankItems fmt File1 l1
    showFile2 = showNonBlankItems fmt File2 l2

failure :: String -> IO a
failure s = IO.hPutStrLn IO.stderr s
  >> E.exitWith (E.ExitFailure 2)

showNonBlankItems
  :: (L.Amount L.Qty -> X.Text)
  -> File
  -> [NonBlankItem]
  -> IO ()
showNonBlankItems fmt f ls =
  mapM_ (showNonBlankItem fmt f) ls

showNonBlankItem
  :: (L.Amount L.Qty -> X.Text)
  -> File
  -> NonBlankItem
  -> IO ()
showNonBlankItem fmt f nbi = maybe e TIO.putStr
  (renderNonBlankItem fmt f nbi)
  where
    e = failure $ "could not render item: " ++ show nbi


-- | Returns a pair p, where fst p is the items that appear in file1
-- but not in file2, and snd p is items that appear in file2 but not
-- in file1.
doDiffs
  :: [C.LedgerItem]
  -> [C.LedgerItem]
  -> ([NonBlankItem], [NonBlankItem])
doDiffs l1 l2 = (r1, r2)
  where
    mkNbList = mapMaybe toNonBlankItem
    (nb1, nb2) = (mkNbList l1, mkNbList l2)
    df = deleteFirstsBy clonedNonBlankItem
    (r1, r2) = (nb1 `df` nb2, nb2 `df` nb1)

-- | Returns a tuple with the first filename, the second filename, and
-- an indication of which differences to show.
parseCommandLine :: IO (String, String, DiffsToShow)
parseCommandLine = do
  as <- M.simpleHelpVersion help (Ly.version PPB.version)
        allOpts M.Intersperse
        (return . Filename)
  let toFilename a = case a of
        Filename s -> Just s
        _ -> Nothing
  (fn1, fn2) <- case mapMaybe toFilename as of
    x:y:[] -> return (x, y)
    _ -> failure "penny-diff: error: you must supply two filenames."
  let getDiffs
        | ((ArgFile File1) `elem` as)
          && ((ArgFile File2) `elem` as) = BothFiles
        | ((ArgFile File1) `elem` as) = File1Only
        | ((ArgFile File2) `elem` as) = File2Only
        | otherwise = BothFiles
  return (fn1, fn2, getDiffs)
