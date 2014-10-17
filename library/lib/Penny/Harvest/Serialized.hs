module Penny.Harvest.Serialized where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import qualified Penny.Harvest.Zoned as Zoned
import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fdbl
import qualified Penny.Harvest.Zoned.Item as Locate.Item
import qualified Penny.Harvest.Serialized.State as State
import qualified Penny.Harvest.Zoned.Located as Located
import qualified Penny.Harvest.Serialized.Item as Item

data T = T { toSeq :: Seq (Located.T Item.T) }
  deriving (Eq, Ord, Show)

fromZoned :: Seq Zoned.T -> Seq T
fromZoned
  = fmap T
  . globals
  . fmap locals
  . fmap Zoned.items


fwdGlobal
  :: Seq (Seq (State.T (State.T (Located.T Item.T))))
  -> (Seq (Seq (State.T (Located.T Item.T))), (Int, Int))
fwdGlobal = Fdbl.foldl f (S.empty, (0,0))
  where
    f (sqs, indexes) sq = (sqs |> sq', indexes')
      where
        (sq', indexes') = State.runState (Tr.sequence sq) indexes

revGlobal
  :: Seq (Seq (State.T (Located.T Item.T)))
  -> (Int, Int)
  -> Seq (Seq (Located.T Item.T))
revGlobal startSq startPair =
  fst $ Fdbl.foldl f (S.empty, startPair) startSq
  where
    f (sqs, indexes) sq = (sqs |> sq', indexes')
      where
        (sq', indexes') = State.runState (Tr.sequence sq) indexes

globals
  :: Seq (Seq (State.T (State.T (Located.T Item.T))))
  -> Seq (Seq (Located.T Item.T))
globals = uncurry revGlobal . fwdGlobal


locals
  :: Seq (Located.T Locate.Item.T)
  -> Seq (State.T (State.T (Located.T Item.T)))
locals = uncurry revLocal . fwdLocal

fwdLocal
  :: Seq (Located.T Locate.Item.T)
  -> ( Seq (State.T (State.T (State.T (Located.T Item.T))))
     , (Int, Int))
fwdLocal sq =
  let stateSeq = Tr.mapM Item.harvest sq
  in State.runState stateSeq (0,0)

revLocal
  :: Seq (State.T (State.T (State.T (Located.T Item.T))))
  -> (Int, Int)
  -> Seq (State.T (State.T (Located.T Item.T)))
revLocal sq start =
  let stateSeq = Tr.sequence sq
  in fst $ State.runState stateSeq start

