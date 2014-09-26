module Penny.Harvest.Serialize.Packages where

import qualified Penny.Harvest.Serialize.Package as Package
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import qualified Penny.Harvest.Locate.Packages as Locate.Packages
import qualified Penny.Harvest.Locate.Package as Locate.Package
import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fdbl
import qualified Penny.Harvest.Locate.Item as Locate.Item
import qualified Penny.Harvest.Serialize.State as State
import qualified Penny.Harvest.Locate.Located as Located
import qualified Penny.Harvest.Serialize.Item as Item

data T = T { toSeq :: Seq Package.T }
  deriving (Eq, Ord, Show)

fromLocatePackages :: Locate.Packages.T -> T
fromLocatePackages (Locate.Packages.T packs)
  = T
  . S.zipWith Package.T (fmap Locate.Package.clxn packs)
  . globals
  . fmap locals
  . fmap Locate.Package.items
  $ packs


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

