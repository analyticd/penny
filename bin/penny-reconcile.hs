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
  as <- MA.simpleWithHelp help MA.Intersperse allOpts (fmap return posArg)
  let opts = foldr ($) (return (), []) as
  fst opts
  led <- C.open . snd $ opts
  let led' = map (either (Left . changeTransaction) Right) led
      rend = fromJust $ mapM (C.item groupSpecs) (map C.stripMeta led')
  mapM_ TIO.putStr rend

