module Penny.Copper.Convert.Collect where

import qualified Penny.Copper.Tree.Memo.Transaction as TM
import qualified Penny.Copper.Tree.Memo.Posting as PM
import qualified Penny.Copper.Convert.Locate as L
import qualified Penny.Copper.Tree.TopLine as T
import qualified Penny.Copper.Tree.Posting as T
import Data.Sequence
import qualified Data.Sequence as S
import qualified Penny.Copper.Convert.Serialize as Z
import Penny.Serial

newtype TopLineMemos = TopLineMemos
  { unTopLineMemos :: Seq (L.Located TM.Memo) }
  deriving (Eq, Ord, Show)

data State
  = Clear
  -- ^ Not currently in posting or transaction; awaiting next input
  | TopLineMemos (Seq (L.Located TM.Memo))
  -- ^ Currently in a TopLineMemo
  | AfterTopLine (Seq (L.Located TM.Memo))
            (L.Located T.TopLine)
            Serial
            Serial
  -- ^ Just saw a TopLine, waiting for a Posting
  | AfterPosting (Seq (L.Located TM.Memo))
                 ((L.Located T.TopLine), Serial, Serial)
                 (Seq ( L.Located T.Posting
                      , Serial
                      , Serial
                      , Seq (L.Located PM.Memo)))
                 (L.Located T.Posting, Serial, Serial)
                 (Seq (L.Located PM.Memo))
  -- ^ At least one posting has already been seen; currently
  -- processing additional postings
  deriving (Eq, Ord, Show)

data Error
  = PostingWithoutTopLine
  | TopLineMemoWithoutTopLine
  | PostingMemoWithoutPosting
  deriving (Eq, Ord, Show)

data Packet = Packet
  { pkTopLineMemo :: Seq (L.Located TM.Memo)
  , pkTopLine :: (L.Located T.TopLine, Serial, Serial)
  , pkPostings :: Seq ( L.Located T.Posting
                      , Serial
                      , Serial
                      , Seq (L.Located PM.Memo))
  } deriving (Eq, Ord, Show)

data Good
  = NoOutput
  | Output Packet
  deriving (Eq, Ord, Show)

process
  :: L.Located Z.Item2
  -> State
  -> (State, Either (L.Located Error) Good)
process (L.Located loc itm) st = case st of
  Clear -> case itm of
    Z.Item2TxnMemo m ->
      ( TopLineMemos (singleton (L.Located loc m))
                     , Right NoOutput)
    Z.Item2TopLine tl s1 s2 ->
      ( AfterTopLine S.empty (L.Located loc tl) s1 s2, Right NoOutput )

    Z.Item2Posting _ _ _ ->
      (Clear, Left (L.Located loc PostingWithoutTopLine))

    Z.Item2PstgMemo _ ->
      (Clear, Left (L.Located loc PostingMemoWithoutPosting))

  TopLineMemos sq -> case itm of
    Z.Item2TxnMemo m ->
      (TopLineMemos (sq |> L.Located loc m), Right NoOutput)
    Z.Item2TopLine tl s1 s2 ->
      ( AfterTopLine S.empty (L.Located loc tl) s1 s2
      ,  Right NoOutput)
    Z.Item2Posting _ _ _ ->
      ( Clear, Left (L.Located loc PostingWithoutTopLine))
    Z.Item2PstgMemo _ ->
      ( Clear, Left (L.Located loc PostingMemoWithoutPosting))

  AfterTopLine ms tl s1 s2 -> case itm of
    Z.Item2TxnMemo m ->
      ( TopLineMemos (singleton (L.Located loc m)), out )
    Z.Item2TopLine tl' s1' s2' ->
      ( AfterTopLine S.empty (L.Located loc tl') s1' s2', out )
    Z.Item2Posting p s1' s2' ->
      ( AfterPosting ms (tl, s1, s2) S.empty
                     (L.Located loc p, s1', s2') S.empty
      , Right NoOutput)
    Z.Item2PstgMemo _ ->
      ( Clear, Left (L.Located loc PostingMemoWithoutPosting ))
    where
      out = Right (Output (Packet ms (tl, s1, s2) S.empty))
