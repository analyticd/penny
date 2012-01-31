module Penny.Bundles where

import Penny.Family.Child ( Child )
import Penny.Family.Family ( Family )
import qualified Penny.Family as F
import qualified Penny.Groups.AtLeast2 as A2
import qualified Penny.Meta.TopLine as MTL
import qualified Penny.Meta.Posting as MP
import qualified Penny.Posting as P
import Penny.Posting.TopLine ( TopLine )

import Data.Text ( Text )

data TopLineWithMeta = TopLineWithMeta { topLine :: TopLine
                                       , line :: MTL.Line }
                       deriving Show

data PostingWithMeta = PostingWithMeta { posting :: P.Posting
                                       , meta :: MP.Meta }
                       deriving Show

data TransactionWithMeta =
  TransactionWithMeta
  { unTransactionWithMeta :: Family TopLineWithMeta PostingWithMeta }
  deriving Show

data Filename = Filename { unFilename :: Text }
                deriving Show

data PostingRecord =
  PostingRecord
  { filename :: Filename
  , record :: Child TopLineWithMeta PostingWithMeta }
  deriving Show

familyWithMeta ::
  P.Transaction
  -> Family MTL.Line MP.Meta
  -> TransactionWithMeta
familyWithMeta wt m = let
  t = P.unTransaction wt
  f = F.mergeWith TopLineWithMeta PostingWithMeta t m
  in TransactionWithMeta f

postingsWithMeta :: 
  Filename
  -> TransactionWithMeta
  -> [PostingRecord]
postingsWithMeta f (TransactionWithMeta t) =
  map (PostingRecord f) . A2.flatten . F.children $ t
