module Penny.Harvest.Transform.Optimus where

import qualified Penny.Core.Clxn as Clxn
import Data.Sequence (Seq)
import qualified Penny.Harvest.Collect.Error as Collect.Error
import qualified Penny.Harvest.Collect.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collect.AfterPosting as AfterPosting
import qualified Penny.Harvest.Transform.Error as Error
import qualified Penny.Core.Transaction as Transaction
import qualified Penny.Harvest.Collect.Result as Result
import qualified Data.Traversable as Tr
import qualified Penny.Harvest.Collect.PostingBox as PostingBox

data T = T
  { clxn :: Clxn.T
  , collectionError :: Collect.Error.T
  , transactions :: Seq (Either Error.T Transaction.T)
  } deriving (Eq, Ord, Show)

fromResult :: Result.T -> T
fromResult (Result.T clx ers gs) = T clx ers txns
  where
    txns = fmap f gs
    f (Left afterTopLine) = case AfterTopLine.toCore afterTopLine of
      Left e -> Left $ Error.TopLine e afterTopLine
      Right fn -> Right $ Transaction.noPostings (fn clx)
    f (Right afterPosting) = afterPostingToTransaction afterPosting clx

afterPostingToTransaction
  :: AfterPosting.T
  -> Clxn.T
  -> Either Error.T Transaction.T
afterPostingToTransaction afterPosting clx = do
  let treeTl = AfterPosting.topLine afterPosting
  fTl <- case AfterTopLine.toCore treeTl of
    Left e -> Left $ Error.TopLine e treeTl
    Right g -> Right g
  let tl = fTl clx
      procPstg p = case PostingBox.toCore p of
        Left e -> Left $ Error.Posting e p
        Right g -> Right g
  pstgs <- Tr.mapM procPstg . AfterPosting.postings $ afterPosting
  either (Left . Error.Transaction tl pstgs) Right
    $ Transaction.fromPostings tl pstgs
