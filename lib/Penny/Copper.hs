-- | Copper - the Penny parser.
--
-- The parse functions in this module only accept lists of files
-- rather than individual files because in order to correctly assign
-- the global serials a single function must be able to see all the
-- transactions, not just the transactions in a single file.
module Penny.Copper
  (
  -- * Convenience functions to read and parse files
  open

  -- * Types for things found in ledger files
  , module Penny.Copper.Interface

  -- * Rendering
  , R.GroupSpec(..)
  , R.GroupSpecs(..)
  , R.item


  ) where

import Control.Monad (when, replicateM_)
import Control.Applicative (pure, (*>), (<$>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as F
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import qualified Penny.Copper.Parsec as CP
import Penny.Copper.Interface

import qualified Penny.Lincoln as L
import qualified Penny.Copper.Render as R
import System.Console.MultiArg.GetArgs (getProgName)
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Penny.Steel.Sums as S

-- | Reads and parses the given files. If any of the files is @-@,
-- reads standard input. If the list of files is empty, reads standard
-- input. IO errors are not caught. Parse errors are printed to
-- standard error and the program will exit with a failure.
open :: [String] -> IO [L.Transaction]
open = undefined

type WithFilePosting
  = (ParsedTopLine, L.Ents (L.PostingCore, L.PostingLine, L.FilePosting))

addFilePosting
  :: [S.S4 a b c ParsedTxn]
  -> [S.S4 a b c WithFilePosting]
addFilePosting = L.serialSomeItems f where
  f i = case i of
    S.S4a x -> Left (S.S4a x)
    S.S4b x -> Left (S.S4b x)
    S.S4c x -> Left (S.S4c x)
    S.S4d txn -> Right (fmap S.S4d g)
      where
        g ser = ( ptParsedTopLine txn
                , fmap (\(c, l) -> (c, l, L.FilePosting ser))
                  . ptEnts $ txn)

type WithFileTransaction
  = ( (ParsedTopLine, L.FileTransaction)
    , L.Ents (L.PostingCore, L.PostingLine, L.FilePosting))

addFileTransaction
  :: [S.S4 a b c WithFilePosting]
  -> [S.S4 a b c WithFileTransaction]
addFileTransaction = L.serialSomeItems f where
  f i = case i of
    S.S4a x -> Left (S.S4a x)
    S.S4b x -> Left (S.S4b x)
    S.S4c x -> Left (S.S4c x)
    S.S4d txn -> Right (fmap S.S4d g) where
      g ser = ((fst txn, L.FileTransaction ser), snd txn)
