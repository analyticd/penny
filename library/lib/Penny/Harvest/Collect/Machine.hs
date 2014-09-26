module Penny.Harvest.Collect.Machine where

import qualified Penny.Harvest.Locate.Located as Located
import qualified Penny.Harvest.Serialize.Item as Item
import qualified Penny.Harvest.Serialize.Package as Package
import qualified Penny.Harvest.Serialize.Packages as Packages
import qualified Penny.Harvest.Collect.State as State
import qualified Penny.Harvest.Collect.Error as Error
import qualified Penny.Harvest.Collect.Good as Good
import qualified Penny.Harvest.Collect.TransactionMemos as TransactionMemos
import qualified Data.Sequence as S
import Data.Sequence (Seq, (|>), ViewL(..))
import qualified Penny.Harvest.Collect.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collect.AfterPosting as AfterPosting
import qualified Penny.Harvest.Collect.PostingBox as PostingBox
import qualified Penny.Harvest.Collect.Result as Result

process
  :: Located.T Item.T
  -> State.T
  -> (State.T, Either (Located.T Error.T) Good.T)

process (Located.T loc (Item.MemoT memo)) State.Empty =
  ( State.TxnMemos . TransactionMemos.T
    . S.singleton . Located.T loc $ memo
  , Right Good.NoOutput )

process (Located.T loc (Item.MemoT memo)) (State.TxnMemos tm) =
  ( State.TxnMemos . TransactionMemos.addMemo tm $ (Located.T loc memo)
  , Right Good.NoOutput
  )

process (Located.T loc (Item.MemoT m)) (State.AfterTopLine atl) =
  ( State.TxnMemos $ TransactionMemos.T (S.singleton (Located.T loc m))
  , Right $ Good.TopLine atl
  )

process (Located.T loc (Item.MemoT tm)) (State.AfterPosting afp) =
  ( State.TxnMemos . TransactionMemos.T . S.singleton
    . Located.T loc $ tm
  , Right . Good.Postings $ afp )

process (Located.T loc (Item.TopLine tl local global)) State.Empty =
  ( State.AfterTopLine $ AfterTopLine.T
    (TransactionMemos.T S.empty) (Located.T loc tl)
    local global
  , Right Good.NoOutput
  )

process (Located.T loc (Item.TopLine tl local global))
  (State.TxnMemos txm) =
  ( State.AfterTopLine $ AfterTopLine.T txm (Located.T loc tl)
      local global
  , Right Good.NoOutput
  )

process (Located.T loc (Item.TopLine tl local global))
  (State.AfterTopLine atl) =
  ( State.AfterTopLine $ AfterTopLine.T (TransactionMemos.T S.empty)
      (Located.T loc tl) local global
  , Right $ Good.TopLine atl
  )

process (Located.T loc (Item.TopLine tl local global))
  (State.AfterPosting afp) =
  ( State.AfterTopLine $ AfterTopLine.T
      (TransactionMemos.T S.empty)
      (Located.T loc tl)
      local global
  , Right . Good.Postings $ afp
  )

process (Located.T loc (Item.Posting _ _ _)) State.Empty =
  ( State.Empty, Left (Located.T loc Error.PostingWithoutTopLine ))

process (Located.T loc (Item.Posting _ _ _))
  (State.TxnMemos _) =
  ( State.Empty, Left (Located.T loc Error.PostingWithoutTopLine ))

process (Located.T loc (Item.Posting p local global))
  (State.AfterTopLine atl) =
  ( State.AfterPosting $ AfterPosting.T atl S.empty pb
  , Right Good.NoOutput
  )
  where
    pb = PostingBox.T (Located.T loc p) local global S.empty

process (Located.T loc (Item.Posting p local global))
  (State.AfterPosting afp) =
  ( State.AfterPosting $ AfterPosting.T
      (AfterPosting.topLine afp)
      ((AfterPosting.postings afp) |> AfterPosting.last afp)
      (PostingBox.T (Located.T loc p) local global S.empty)
  , Right Good.NoOutput
  )

process (Located.T loc (Item.MemoP _)) State.Empty =
  ( State.Empty, Left (Located.T loc Error.PostingMemoWithoutPosting))

process (Located.T loc (Item.MemoP _)) (State.TxnMemos _) =
  ( State.Empty, Left (Located.T loc Error.PostingMemoWithoutPosting))

process (Located.T loc (Item.MemoP _)) (State.AfterTopLine _) =
  ( State.Empty, Left (Located.T loc Error.PostingMemoWithoutPosting))

process (Located.T loc (Item.MemoP m)) (State.AfterPosting afp) =
  ( State.AfterPosting
    $ afp { AfterPosting.last = (AfterPosting.last afp)
            { PostingBox.memos = (PostingBox.memos
                (AfterPosting.last afp)) |>
                Located.T loc m } }
  , Right Good.NoOutput
  )

destroy :: State.T -> Either Error.T Good.T
destroy State.Empty = Right Good.NoOutput
destroy (State.TxnMemos _) = Left Error.TransactionMemoWithoutTopLine
destroy (State.AfterTopLine atl) = Right $ Good.TopLine atl
destroy (State.AfterPosting app) = Right $ Good.Postings app

collectPackage :: Package.T -> Result.T
collectPackage (Package.T cl sqnc) = Result.T cl es fin gs
  where
    (fin, es, gs) = runMachine sqnc

collectPackages :: Packages.T -> Seq Result.T
collectPackages = fmap collectPackage . Packages.toSeq

runMachine
  :: Seq (Located.T Item.T)
  -> ( Maybe Error.T
     , Seq (Located.T Error.T)
     , Seq (Either AfterTopLine.T AfterPosting.T) )
runMachine items = (finalErr, errors, allGoods)
  where
    (stFin, errors, goods) = go items State.Empty S.empty S.empty
    go sq st es gs = case S.viewl sq of
      EmptyL -> (st, es, gs)
      x :< xs -> go xs st' es' gs'
        where
          (st', ei) = process x st
          (es', gs') = case ei of
            Left err -> (es |> err, gs)
            Right g -> (es, Good.appendToSeq gs g)
    (finalErr, allGoods) = case destroy stFin of
      Left e -> (Just e, goods)
      Right g -> (Nothing, Good.appendToSeq goods g)

