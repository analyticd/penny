module Main where

import qualified Control.Exception as CEx
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified System.Console.MultiArg as M
import qualified Penny.Lincoln as L
import qualified Penny.Copper as C
import qualified Penny.Copper.Transaction as CT
import qualified Penny.Copper.Price as CP
import qualified Penny.Copper.Comments as CC
import qualified Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified System.Exit as E
import qualified System.IO as IO

data CopperOptions = CopperOptions
  { defaultTimeZone :: Cop.DefaultTimeZone
  , groupingSpecs :: (Cop.GroupingSpec, Cop.GroupingSpec)
  , radGroup :: Cop.RadGroup
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

toNonBlankItem :: C.Item -> Maybe NonBlankItem
toNonBlankItem i = case i of
  C.Transaction t -> Just (Transaction t)
  C.Price p -> Just (Price p)
  C.Comment c -> Just (Comment c)
  _ -> Nothing

renderNonBlankItem :: CopperOptions -> NonBlankItem -> Maybe X.Text
renderNonBlankItem (CopperOptions dtz gs rg) n = case n of
  Transaction t -> CT.render dtz gs rg
  Price p -> CP.render dtz gs rg
  Comment c -> CC.render c

runPennyDiff :: CopperOptions -> IO ()
runPennyDiff co@(CopperOptions dtz gs rg) = undefined

failure :: String -> IO ()
failure s = IO.hPutStrLn s >> E.exitWith (E.ExitFailure 2)

-- | Returns a pair p, where fst p is the items that appear in file1
-- but not in file2, and snd p is items that appear in file2 but not
-- in file1.
doDiffs
  :: Cop.Ledger
  -> Cop.Ledger
  -> ([NonBlankItem], [NonBlankItem])
doDiffs l1 l2 

parseFile :: CopperOptions -> String -> IO Cop.Ledger
parseFile (CopperOptions dtz _ rg) s = do
  eiTxt <- CEx.try $ TIO.readFile s
  txt <- case eiTxt of
    Left (e :: IOError) -> failure (show e)
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

