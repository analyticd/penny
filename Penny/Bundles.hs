module Penny.Bundles where

import Penny.Family.Child ( Child )
import Penny.Family.Family ( Family )
import qualified Penny.Family as F
import qualified Penny.Groups.AtLeast2 as A2
import qualified Penny.Meta as M
import qualified Penny.Posting as P
import Penny.Posting.TopLine ( TopLine )


data TopLineWithMeta = TopLineWithMeta { topLine :: TopLine
                                       , line :: M.Line }
                       deriving Show

data PostingWithMeta = PostingWithMeta { posting :: P.Posting
                                       , meta :: M.Meta }
                       deriving Show

data TransactionWithMeta =
  TransactionWithMeta
  { unTransactionWithMeta :: Family TopLineWithMeta PostingWithMeta }
  deriving Show

data PostingRecord =
  PostingRecord
  { filename :: M.Filename
  , record :: Child TopLineWithMeta PostingWithMeta }
  deriving Show

familyWithMeta ::
  P.Transaction
  -> Family M.Line M.Meta
  -> TransactionWithMeta
familyWithMeta wt m = let
  t = P.unTransaction wt
  f = F.mergeWith TopLineWithMeta PostingWithMeta t m
  in TransactionWithMeta f

postingsWithMeta :: 
  M.Filename
  -> TransactionWithMeta
  -> [PostingRecord]
postingsWithMeta f (TransactionWithMeta t) =
  map (PostingRecord f) . A2.flatten . F.children $ t
