-- | Copper - the Penny parser.
--
-- The parse functions in this module only accept lists of files
-- rather than individual files because in order to correctly assign
-- the global serials a single function must be able to see all the
-- transactions, not just the transactions in a single file.
--
-- Some notes about Copper and performance:
--
-- Running Penny on the datasets I typically use takes about two
-- seconds. This does not seem very long on paper, and indeed it isn't
-- very long, but it would be nice if this were
-- instantaneous. Profiles consistently show that the most
-- time-consuming part of running Penny is the Parsec parse of the
-- incoming data. After eliminating the Parsec phase, the profile is
-- not showing any parts of the program whose runtime could be
-- shortened easily--the time is spent scattered amongst many
-- functions.
--
-- So the clear place to hunt for performance improvements is in the
-- Parsec phase. And, indeed, I have tried many things to improve this
-- phase. I tried using a parser based on Happy and Alex rather than
-- Parsec; this code is tagged in the Git repository, though it is so
-- old that many of the other data structures in Penny have since
-- changed. Happy and Alex did not yield any significant performance
-- improvement. As I recall, between Parsec and Happy/Alex, one was a
-- little faster but used more memory, though I can't remember which
-- was which.
--
-- The problem with using Happy and Alex is that it is a bit harder to
-- test and to maintain. Each Parsec parser is freestanding and can be
-- tested on its own; doing this with Happy would be harder. Happy
-- parsers also are not written in Haskell, though I'm not sure this
-- is a disadvantage. And, of course an advantage to Happy is that it
-- warns you if your grammar is ambiguous; Parsec will only reveal
-- this through usage or through meticulous testing.
--
-- It isn't worth using Happy/Alex in Penny because of the negligible
-- performance difference. Parsec has much better error messages than
-- Happy/Alex, which turns out to be critically important.
--
-- Another thing I tried was using Attoparsec, which bills itself as
-- being faster. The speed improvements were negligible, and
-- Parsec error messages are much better than those in Attoparsec. I
-- would have been willing to maintain a Parsec and an Attoparsec
-- parser if the latter were faster. Penny could parse with Attoparsec
-- first and, if that fails, use Parsec and use its error message. But
-- Attoparsec was so negligibly faster that I did not think this
-- worthwhile.
--
-- Another thing I tried was using the @binary@ package to serialize
-- the data in binary form. This shaved off a fair amont of run
-- time. But Penny still did not feel instantaneous--run time probably
-- dropped by about 40 percent, which is significant. The big
-- disadvantage to using binary is that you then need to get
-- plain-text ledger files into binary form, save them, and then use
-- the binary form if it is up to date. Doing this manually imposes a
-- big burden on the user to convert plain text to binary. Doing it
-- automatically could work but would be a lot of code. And then, you
-- would need to factor converstion time into the performance
-- comparison. Again, not worth it for the performance improvement
-- involved.
--
-- Probably the best performance improvement would come from putting
-- the whole ledger into SQLite. This would, however, run into the
-- same problems that exist with using a binary format: you need to
-- convert from plain text, or perhaps write an editor to change the
-- binary natively.  I'm not eager to write an editor (we already have
-- Emacs).  Furthermore, using SQLite would likely require a
-- significant re-engineering of Penny.
--
-- So, Penny continues to use the simplest, most obvious solution--a
-- Parsec parser--not from inertia or because Parsec is the default
-- choice; rather, Parsec so far has proven to be the best solution to
-- this problem.
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
open ss
  | null ss = fmap (parsedToWrapped . (:[])) CP.parseStdinOnly
  | otherwise = fmap parsedToWrapped $ mapM CP.parseFromFilename ss

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

