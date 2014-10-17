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
-- 'Penny.Harvest.Collect.Machine.collectPackages'.
--
-- 4.  Transform.  Converts Penny.Copper.Tree top lines and postings
-- to mainline top lines and postings, and create balanced
-- transactions.  The main function is
-- 'Penny.Harvest.Transform.Optimus.fromResult'.
--
-- This module gathers all the functions together into a single
-- function, 'harvest'.

module Penny.Harvest where

import qualified Penny.Core.Clxn as Clxn
import qualified Data.Text as X
import Data.Sequence (Seq)
import qualified Penny.Core.Transaction as Transaction
import qualified Penny.Harvest.Error as Error

harvest
  :: Seq (Clxn.T, X.Text)
  -- ^ A sequence of each collection name, paired with the text
  -- containing the ledger file data (most likely read in from disk.)

  -> Either Error.T (Seq Transaction.T)
  -- ^ If any step along the way fails, an error is returned;
  -- otherwise, a sequence of all transactions is returned.
harvest = undefined
