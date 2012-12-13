module Main where

import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import Control.Monad (guard)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import qualified System.Console.MultiArg as MA
import System.Exit (exitSuccess)

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

help :: String
help = unlines
  [ "usage: penny-reconcile [-h] FILE..."
  , "Finds all transactions and postings bearing a \"C\" flag"
  , "and changes them to a \"R\" flag in the listed FILEs."
  , "If no FILE, or if FILE is -, read standard input."
  , ""
  , "Output is printed to standard output. Input files are not"
  , "changed."
  , ""
  , "Options:"
  , "  -h, --help Show help and exit."
  ]

toPosArg :: Arg -> Maybe String
toPosArg a = case a of
  APosArg s -> Just s
  _ -> Nothing

data Arg
  = AHelp
  | APosArg String
  deriving (Eq, Show)

data Opts
  = NeedsHelp
  | DoIt [String]
  deriving Show


parseArgs :: [String] -> Opts
parseArgs ss =
  let opts = [ MA.OptSpec ["help"] "h" (MA.NoArg AHelp) ]
  in case MA.simple MA.Intersperse opts APosArg ss of
      Ex.Exception e -> error . show $ e
      Ex.Success g ->
        if any (== AHelp) g
        then NeedsHelp
        else DoIt $ mapMaybe toPosArg g

groupSpecs :: C.GroupSpecs
groupSpecs = C.GroupSpecs C.NoGrouping C.NoGrouping

main :: IO ()
main = do
  os <- fmap parseArgs MA.getArgs
  ls <- case os of
    NeedsHelp -> putStrLn help >> exitSuccess
    DoIt ss -> return ss
  led <- C.openStdin ls
  let led' = C.mapLedger (C.mapItem id id changeTransaction) led
      rend = fromJust $ C.ledger groupSpecs led'
  TIO.putStr rend
