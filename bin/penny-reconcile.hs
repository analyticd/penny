module Main where

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Text as X
import Control.Monad (guard)
import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Steel.Sums as S
import qualified System.Console.MultiArg as MA
import qualified Paths_penny_bin as PPB


-- | Changes a posting to mark it reconciled, if it was already marked
-- as cleared.
changePosting :: L.PostingData -> L.PostingData
changePosting p = fromMaybe p $ do
  let c = L.pdCore p
  fl <- L.pFlag c
  guard (L.unFlag fl == X.singleton 'C')
  let fl' = L.Flag . X.singleton $ 'R'
      c' = c { L.pFlag = Just fl' }
  return p { L.pdCore = c' }

-- | Changes a TopLine to mark it as reconciled, if it was already
-- marked as cleared.
changeTopLine :: L.TopLineData -> L.TopLineData
changeTopLine t = fromMaybe t $ do
  let c = L.tlCore t
  fl <- L.tFlag c
  guard (L.unFlag fl == X.singleton 'C')
  let fl' = L.Flag . X.singleton $ 'R'
      c' = c { L.tFlag = Just fl' }
  return t { L.tlCore = c' }

changeTransaction :: L.Transaction -> L.Transaction
changeTransaction (L.Transaction (tl, es)) =
  L.Transaction (changeTopLine tl, fmap changePosting es)

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " [options] FILE..."
  , "Finds all transactions and postings bearing a \"C\" flag"
  , "and changes them to a \"R\" flag in the listed FILEs."
  , "If no FILE, or if FILE is -, read standard input."
  , ""
  , "Output is printed to standard output. Input files are not"
  , "changed."
  , ""
  , "Options:"
  , "  --output FILENAME, -o FILENAME"
  , "    send output to FILENAME rather than standard output"
  , "    (multiple -o options are allowed; use \"-\" for standard"
  , "     output)"
  , "  -h, --help - Show help and exit."
  , "  --version  - Show version and exit"
  ]

groupSpecs :: C.GroupSpecs
groupSpecs = C.GroupSpecs C.NoGrouping C.NoGrouping

type ShowVer = IO ()
type Printer = X.Text -> IO ()
type PosArg = String
type Arg = S.S3 ShowVer Printer PosArg

allOpts :: [MA.OptSpec Arg]
allOpts =
  [ fmap S.S3a $ Ly.version PPB.version
  , fmap S.S3b Ly.output
  ]

main :: IO ()
main = do
  as <- MA.simpleWithHelp help MA.Intersperse allOpts (fmap return S.S3c)
  let (showVer, printers, posArgs) = S.partitionS3 as
  sequence_ showVer
  led <- C.open posArgs
  let led' = map (S.mapS4 changeTransaction id id id) led
      rend = fromJust $ mapM (C.item groupSpecs) (map C.stripMeta led')
  let txt = X.concat rend in txt `seq` (Ly.processOutput printers txt)

