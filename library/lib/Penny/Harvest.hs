-- | Modules in this hierarchy convert items from "Penny.Tree" to the
-- core data types in "Penny.Core"; that is, they \"harvest\" trees.
--
-- There are five steps to this process:
--
-- 1.  Zoning.  Associates each line in each collection with its
-- associated line number.  Retains only posting memo lines,
-- transaction memo lines, transaction lines, and posting lines;
-- discards all other lines.  The main function is
-- 'Penny.Harvest.Zoned.fromLines'.
--
-- 2.  Serialization.  Associates posting lines with global and
-- collection serials, and top lines with global and collection
-- serials.  The main function is
-- 'Penny.Harvest.Serialized.fromZoned'.
--
-- Each of the following steps can fail.  If a step fails, keep the
-- failure message, but proceed to act upon as many more items as
-- possible.
--
-- 3.  Collect.  Gathers top lines with their memo lines and postings
-- with their memo lines, and the top lines with their associated
-- postings.  The main function is
-- 'Penny.Harvest.Collected.fromSerializedItems'.
--
-- 4.  Transform.  Converts Penny.Copper.Tree top lines and postings
-- to mainline top lines and postings, and create balanced
-- transactions.  The main function is
-- 'Penny.Harvest.Transformed.transform'.
--
-- This module gathers all the functions together into a single
-- function, 'parseAndHarvest'.  In addition to performing the steps
-- above (which comprise harvesting), 'parseAndHarvest' also parses
-- the text of each ledger using Parsec.

module Penny.Harvest where

import qualified Penny.Core.Clxn as Clxn
import qualified Data.Text as X
import Data.Sequence (Seq, ViewL(..), (<|))
import qualified Data.Sequence as Seq
import qualified Penny.Harvest.Error.Detail as Detail
import qualified Penny.Core.Transaction as Transaction
import qualified Penny.Harvest.Error as Error
import qualified Penny.Harvest.Error.Log as Error.Log
import qualified Penny.Harvest.Collected.Error as Collected.Error
import qualified Penny.Harvest.Transformed as Transformed
import qualified Penny.Tree.Line as Line
import qualified Penny.Tree.File as File
import qualified Penny.Harvest.Zoned as Zoned
import qualified Penny.Harvest.Serialized as Serialized
import qualified Penny.Harvest.Collected as Collected
import qualified Penny.Harvest.Collected.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collected.AfterPosting as AfterPosting
import qualified Text.Parsec as Parsec
import qualified Data.Traversable as Tr
import Control.Monad
import Prelude hiding (lines)

parseAndHarvest
  :: Seq (Clxn.T, X.Text)
  -- ^ A sequence of each collection name, paired with the text
  -- containing the ledger file data (most likely read in from disk.)

  -> Either (Seq Error.T) (Seq Transaction.T)
  -- ^ If there is any failure, return the errors; otherwise, return
  -- all transactions.
parseAndHarvest sq = Error.Log.toEither $ do
  seqLines <- Tr.sequence . fmap (uncurry parse) $ sq
  let seqZoned = fmap Zoned.fromLines seqLines
      seqSerialized = Serialized.fromZoned seqZoned
      seqLogs = Seq.zipWith collect (fmap fst sq) seqSerialized
  seqCollected <- Tr.sequence seqLogs
  fmap join . Tr.sequence . Seq.zipWith transform (fmap fst sq)
    $ seqCollected

parse
  :: Clxn.T
  -> X.Text
  -> Error.Log.T (Seq Line.T)
parse clxn txt =
  case Parsec.parse File.parser (X.unpack . Clxn.toText $ clxn) txt of
    Left e -> Error.Log.log (Error.T clxn (Detail.Parsec e))
      >> return Seq.empty
    Right (File.T ls _) -> return ls

collect
  :: Clxn.T
  -> Serialized.T
  -> Error.Log.T (Seq (Either AfterTopLine.T AfterPosting.T))
collect clxn (Serialized.T slz) = do
  let Collected.T ers gds = Collected.fromSerializedItems slz
      logIn = Error.Log.log . Error.T clxn . Detail.CollectInline
      logFin Nothing = return ()
      logFin (Just fin) = Error.Log.log . Error.T clxn
        . Detail.CollectFinal $ fin
  _ <- Tr.traverse logIn . Collected.Error.inlines $ ers
  logFin . Collected.Error.final $ ers
  return gds

transform
  :: Clxn.T
  -> Seq (Either AfterTopLine.T AfterPosting.T)
  -> Error.Log.T (Seq Transaction.T)
transform clxn sqEithers = go sqTxns
  where
    Transformed.T sqTxns = Transformed.transform clxn sqEithers
    go sq = case Seq.viewl sq of
      EmptyL -> return Seq.empty
      (Left err) :< xs -> do
        Error.Log.log . Error.T clxn . Detail.Transform $ err
        go xs
      (Right txn) :< xs -> do
        rest <- go xs
        return (txn <| rest)
