module Penny.Copper.Convert.Serialize where

import Penny.Serial
import qualified Penny.Copper.Convert.Locate as L
import qualified Penny.Common as C
import qualified Penny.Copper.Package as P
import Data.Sequence
import qualified Data.Sequence as S
import qualified Penny.Copper.Tree.Memo.Transaction as TM
import qualified Penny.Copper.Tree.Memo.Posting as PM
import qualified Penny.Copper.Tree.Posting as T
import qualified Penny.Copper.Tree.TopLine as T
import qualified Penny.Copper.Tree.File as T
import qualified Penny.Copper.Tree.Line as T
import Data.Sums

serialize :: L.Packages1 -> Packages2
serialize = step1Packages . step0Packages

data Item2
  = Item2TxnMemo TM.Memo
  | Item2TopLine T.TopLine Serial Serial
  -- ^ The global serial and the collection serial
  | Item2Posting T.Posting Serial Serial
  -- ^ The global serial and the collection serial
  | Item2PstgMemo PM.Memo
  deriving (Eq, Ord, Show)

data Package2 = Package2 C.Clxn (Seq (L.Located Item2))
  deriving (Eq, Ord, Show)

newtype Packages2 = Packages2 { unPackages2 :: Seq Package2 }
  deriving (Eq, Ord, Show)

-- | Step 0 - add forward serials
data Step0Item
  = Step0TxnMemo TM.Memo
  | Step0TopLine T.TopLine !Int !Int
  -- ^ Global and collection serials, both forward
  | Step0Posting T.Posting !Int !Int
  -- ^ Global and collection serials, both forward
  | Step0PstgMemo PM.Memo
  deriving (Eq, Ord, Show)

data Step0Package = Step0Package C.Clxn (Seq (L.Located Step0Item))
  deriving (Eq, Ord, Show)

newtype Step0Packages
  = Step0Packages { unStep0Packages :: Seq Step0Package }
  deriving (Eq, Ord, Show)

step0Package
  :: Int
  -- ^ Beginning global serial, top line
  -> Int
  -- ^ Beginning global serial, posting
  -> L.Package1
  -> (Step0Package, Int, Int)
  -- ^ Package, ending global serial (top line),
  -- ending global serial (posting)
step0Package tl pstg (L.Package1 cx sq)
  = (Step0Package cx sq', tl', pstg')
  where
    (sq', tl', pstg') = go sq tl pstg 0 0 S.empty
    go remains gT gP cT cP soFar = case viewl remains of
      EmptyL -> (soFar, gT, gP)
      (L.Located l i) :< xs -> go xs gT' gP' cT' cP' soFar'
        where
          (gT', gP', cT', cP', end) = case i of
            L.Item1TxnMemo x -> (gT, gP, cT, cP, Step0TxnMemo x)
            L.Item1TopLine x ->
              (succ gT, gP, succ cT, cP, Step0TopLine x gT cT)
            L.Item1Posting x ->
              (gT, succ gP, cT, succ cP, Step0Posting x gP cP)
            L.Item1PstgMemo x -> (gT, gP, cT, cP, Step0PstgMemo x)
          soFar' = soFar |> (L.Located l end)

step0Packages :: L.Packages1 -> Step0Packages
step0Packages = Step0Packages . go 0 0 . L.unPackages1
  where
    go gT gP sq = case viewl sq of
      EmptyL -> S.empty
      x :< xs -> p <| go gT' gP' xs
        where
          (p, gT', gP') = step0Package gT gP x

step1Package
  :: Int
  -- ^ Beginning global serial, top line
  -> Int
  -- ^ Beginning global serial, posting
  -> Step0Package
  -> (Package2, Int, Int)
  -- ^ Package, ending global serial (top line), ending global serial
  -- (posting)
step1Package tl pstg (Step0Package cx sq)
  = (Package2 cx sq', tl', pstg')
  where
    (sq', tl', pstg') = go sq tl pstg 0 0 S.empty
    go remains gT gP cT cP soFar = case viewr remains of
      EmptyR -> (soFar, gT, gP)
      xs :> (L.Located l i) -> go xs gT' gP' cT' cP' soFar'
        where
          (gT', gP', cT', cP', end) = case i of
            Step0TxnMemo x -> (gT, gP, cT, cP, Item2TxnMemo x)
            Step0TopLine x gF cF ->
              (succ gT, gP, succ cT, cP,
               Item2TopLine x (Serial gF gT) (Serial cF cT))
            Step0Posting x gF cF ->
              (gT, succ gP, cT, succ cP,
               Item2Posting x (Serial gF gP) (Serial cF cP))
            Step0PstgMemo x -> (gT, gP, cT, cP, Item2PstgMemo x)
          soFar' = (L.Located l end) <| soFar

step1Packages :: Step0Packages -> Packages2
step1Packages = Packages2 . go 0 0 . unStep0Packages
  where
    go gT gP sq = case viewr sq of
      EmptyR -> S.empty
      xs :> x -> go gT' gP' xs |> p
        where
          (p, gT', gP') = step1Package gT gP x
