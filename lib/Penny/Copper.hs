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

import Control.Arrow (second)
import qualified Data.Traversable as Tr
import qualified Penny.Copper.Parsec as CP
import qualified Penny.Steel.Sums as S
import Penny.Copper.Interface
import qualified Penny.Copper.Interface as I

import qualified Penny.Lincoln as L
import qualified Penny.Copper.Render as R

-- | Reads and parses the given files. If any of the files is @-@,
-- reads standard input. If the list of files is empty, reads standard
-- input. IO errors are not caught. Parse errors are printed to
-- standard error and the program will exit with a failure.
open :: [String] -> IO [I.LedgerItem]
open ss = fmap parsedToWrapped $ mapM CP.parse ss

addFilePosting
  :: Tr.Traversable f
  => [S.S4 (a, f b) x y z]
  -> [S.S4 (a, f (L.FilePosting, b)) x y z]
addFilePosting = L.serialNestedItems f where
  f i = case i of
    S.S4a (a, ctnr) ->
      Right ( ctnr
            , (\ser ii -> (L.FilePosting ser, ii))
            , (\res -> S.S4a (a, res))
            )
    S.S4b x -> Left (S.S4b x)
    S.S4c x -> Left (S.S4c x)
    S.S4d x -> Left (S.S4d x)

addFileTransaction
  :: [S.S4 (a, b) x y z]
  -> [S.S4 ((L.FileTransaction, a), b) x y z]
addFileTransaction = L.serialSomeItems f where
  f i = case i of
    S.S4a (a, b) -> Right (\ser -> S.S4a ((L.FileTransaction ser, a), b))
    S.S4b x -> Left (S.S4b x)
    S.S4c x -> Left (S.S4c x)
    S.S4d x -> Left (S.S4d x)

addGlobalTransaction
  :: [S.S4 (a, b) x y z]
  -> [S.S4 ((L.GlobalTransaction, a), b) x y z]
addGlobalTransaction = L.serialSomeItems f where
  f i = case i of
    S.S4a (a, b) -> Right (\ser -> S.S4a ((L.GlobalTransaction ser, a), b))
    S.S4b x -> Left (S.S4b x)
    S.S4c x -> Left (S.S4c x)
    S.S4d x -> Left (S.S4d x)

addGlobalPosting
  :: Tr.Traversable f
  => [S.S4 (a, f b) x y z]
  -> [S.S4 (a, f (L.GlobalPosting, b)) x y z]
addGlobalPosting = L.serialNestedItems f where
  f i = case i of
    S.S4a (a, ctnr) ->
      Right ( ctnr
            , (\ser ii -> (L.GlobalPosting ser, ii))
            , (\res -> S.S4a (a, res))
            )
    S.S4b x -> Left (S.S4b x)
    S.S4c x -> Left (S.S4c x)
    S.S4d x -> Left (S.S4d x)

addFilename
  :: L.Filename
  -> [S.S4 (a, b) x y z]
  -> [S.S4 ((L.Filename, a), b) x y z]
addFilename fn = map f where
  f i = case i of
    S.S4a (a, b) -> S.S4a ((fn, a), b)
    S.S4b x -> S.S4b x
    S.S4c x -> S.S4c x
    S.S4d x -> S.S4d x

addFileSerials
  :: Tr.Traversable f
  => [S.S4 (a, f b) x y z]
  -> [S.S4 ((L.FileTransaction, a), f (L.FilePosting, b)) x y z]
addFileSerials
  = addFilePosting
  . addFileTransaction

addFileData
  :: Tr.Traversable f
  => (L.Filename, [S.S4 (a, f b) x y z])
  -> [S.S4 ((L.Filename, (L.FileTransaction, a)), f (L.FilePosting, b)) x y z]
addFileData = uncurry addFilename . second addFileSerials

addGlobalSerials
  :: Tr.Traversable f
  => [S.S4 (a, f b) x y z]
  -> [S.S4 ((L.GlobalTransaction, a), f (L.GlobalPosting, b)) x y z]
addGlobalSerials
  = addGlobalTransaction
  . addGlobalPosting

addAllMetadata
  :: Tr.Traversable f
  => [(L.Filename, [S.S4 (a, f b) x y z])]
  -> [S.S4 ((L.GlobalTransaction, (L.Filename, (L.FileTransaction, a))),
              f (L.GlobalPosting, (L.FilePosting, b))) x y z]
addAllMetadata
  = addGlobalSerials
  . concat
  . map addFileData

rewrapMetadata
  :: Functor f
  => ( (L.GlobalTransaction, (L.Filename, (L.FileTransaction, I.ParsedTopLine)))
     , f (L.GlobalPosting, (L.FilePosting, (L.PostingCore, L.PostingLine))))
  -> (L.TopLineData, f (L.PostingData))
rewrapMetadata ((gt, (fn, (ft, ptl))), ctr) = (tld, fmap f ctr)
  where
    tld = L.TopLineData
      tlc
      (Just (L.TopLineFileMeta fn (I.ptlTopLineLine ptl)
                                  (fmap snd $ I.ptlMemo ptl)
                             ft))
      (Just gt)
    tlc = L.TopLineCore (I.ptlDateTime ptl) (I.ptlNumber ptl)
                        (I.ptlFlag ptl) (I.ptlPayee ptl)
                        (fmap fst $ I.ptlMemo ptl)
    f (gp, (fp, (pc, pl))) = L.PostingData
      pc
      (Just (L.PostingFileMeta pl fp))
      (Just gp)

parsedToWrapped
  :: [(L.Filename, [I.ParsedItem])]
  -> [I.LedgerItem]
parsedToWrapped = map rewrap . addAllMetadata where
  rewrap i = case i of
    S.S4a x -> S.S4a (L.Transaction . rewrapMetadata $ x)
    S.S4b x -> S.S4b x
    S.S4c x -> S.S4c x
    S.S4d x -> S.S4d x

