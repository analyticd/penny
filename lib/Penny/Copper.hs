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
  => [Either (a, f b) c]
  -> [Either (a, f (L.FilePosting, b)) c]
addFilePosting = L.serialNestedItems f where
  f i = case i of
    Left (a, ctnr) ->
      Right ( ctnr
            , (\ser ii -> (L.FilePosting ser, ii))
            , (\res -> Left (a, res))
            )
    Right b -> Left (Right b)

addFileTransaction
  :: [Either (a, b) c]
  -> [Either ((L.FileTransaction, a), b) c]
addFileTransaction = L.serialSomeItems f where
  f i = case i of
    Right c -> Left (Right c)
    Left (a, b) -> Right (\ser -> Left ((L.FileTransaction ser, a), b))

addGlobalTransaction
  :: [Either (a, b) c]
  -> [Either ((L.GlobalTransaction, a), b) c]
addGlobalTransaction = L.serialSomeItems f where
  f i = case i of
    Right b -> Left (Right b)
    Left (a, b) -> Right (\ser -> Left ((L.GlobalTransaction ser, a), b))

addGlobalPosting
  :: Tr.Traversable f
  => [Either (a, f b) c]
  -> [Either (a, f (L.GlobalPosting, b)) c]
addGlobalPosting = L.serialNestedItems f where
  f i = case i of
    Left (a, ctnr) ->
      Right ( ctnr
            , (\ser ii -> (L.GlobalPosting ser, ii))
            , (\res -> Left (a, res))
            )
    Right b -> Left (Right b)

addFilename
  :: L.Filename
  -> [Either (a, b) c]
  -> [Either ((L.Filename, a), b) c]
addFilename fn = map (either (\(a, b) -> Left ((fn, a), b)) (Right . id))


addFileSerials
  :: Tr.Traversable f
  => [Either (a, f b) c]
  -> [Either ((L.FileTransaction, a), f (L.FilePosting, b)) c]
addFileSerials
  = addFilePosting
  . addFileTransaction

addFileData
  :: Tr.Traversable f
  => (L.Filename, [Either (a, f b) c])
  -> [Either ((L.Filename, (L.FileTransaction, a)), f (L.FilePosting, b)) c]
addFileData = uncurry addFilename . second addFileSerials

addGlobalSerials
  :: Tr.Traversable f
  => [Either (a, f b) c]
  -> [Either ((L.GlobalTransaction, a), f (L.GlobalPosting, b)) c]
addGlobalSerials
  = addGlobalTransaction
  . addGlobalPosting

addAllMetadata
  :: Tr.Traversable f
  => [(L.Filename, [Either (a, f b) c])]
  -> [Either ((L.GlobalTransaction, (L.Filename, (L.FileTransaction, a))),
              f (L.GlobalPosting, (L.FilePosting, b))) c]
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
  rewrap = either (Left . L.Transaction . rewrapMetadata) Right
