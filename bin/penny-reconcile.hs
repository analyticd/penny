module Main where

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import Control.Monad (guard)
import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified System.Console.MultiArg as MA
import qualified Paths_penny_bin as PPB


-- | Changes a posting to mark it reconciled, if it was already marked
-- as cleared.
changePosting :: L.Posting -> L.PostingChangeData
changePosting p = fromMaybe L.emptyPostingChangeData $ do
  fl <- L.pFlag p
  guard (L.unFlag fl == X.singleton 'C')
  let fl' = L.Flag . X.singleton $ 'R'
  return $ L.emptyPostingChangeData { L.pcFlag = Just (Just fl') }

-- | Changes a TopLine to mark it as reconciled, if it was already
-- marked as cleared.
changeTopLine :: L.TopLine -> L.TopLineChangeData
changeTopLine t = fromMaybe L.emptyTopLineChangeData $ do
  fl <- L.tFlag t
  guard (L.unFlag fl == X.singleton 'C')
  let fl' = L.Flag . X.singleton $ 'R'
  return $ L.emptyTopLineChangeData { L.tcFlag = Just (Just fl') }

changeTransaction :: L.Transaction -> L.Transaction
changeTransaction t =
  let fam = L.mapParent changeTopLine
            . L.mapChildren changePosting
            . L.unTransaction
            $ t
  in L.changeTransaction fam t

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
  , "  -h, --help - Show help and exit."
  , "  --version  - Show version and exit"
  ]

groupSpecs :: C.GroupSpecs
groupSpecs = C.GroupSpecs C.NoGrouping C.NoGrouping

-- | The first element if the pair is a no-op if the user does not
-- need to see the version, or an IO action to print the version if
-- the user wants to see it. The second element is the list of command
-- line arguments.
type Opts = (IO (), [String])

allOpts :: [MA.OptSpec (Opts -> Opts)]
allOpts = [ fmap (\act (_, ss) -> (act, ss)) $
            Ly.version PPB.version
          ]

posArg :: String -> Opts -> Opts
posArg s (a, ss) = (a, s:ss)

main :: IO ()
main = do
  as <- MA.simpleWithHelp help MA.Intersperse allOpts posArg
  let opts = foldr ($) (return (), []) as
  fst opts
  led <- C.open . snd $ opts
  let led' = C.mapLedger (C.mapItem id id changeTransaction) led
      rend = fromJust $ C.ledger groupSpecs led'
  TIO.putStr rend

