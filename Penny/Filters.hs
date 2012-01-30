module Penny.Filters where

import Data.Maybe ( catMaybes )
import Data.Text ( Text )

import Penny.Groups.AtLeast2 ( flatten )
import Penny.Family ( mergeWith, children )
import Penny.Family.Family ( Family ( Family ) )
import qualified Penny.Family.Child as C
import qualified Penny.Parser.Item as I
import qualified Penny.Parser.Transaction as T
import qualified Penny.Parser.Posting as PPo
import Penny.Parser.TopLine ( TopLineLine )
import qualified Penny.Posting as P
import qualified Penny.Posting.TopLine as TL


data PostingWithMeta = PostingWithMeta { posting :: P.Posting
                                       , meta :: PPo.Meta }
                       deriving Show

data TopLineWithMeta = TopLineWithMeta { topLine :: TL.TopLine
                                       , topLineLine :: TopLineLine }
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
  , record :: C.Child TopLineWithMeta PostingWithMeta }
  deriving Show

familyWithMeta :: P.Transaction -> T.Meta -> TransactionWithMeta
familyWithMeta wt wm = let
  t = P.unTransaction wt
  m = T.unMeta wm
  f = mergeWith TopLineWithMeta PostingWithMeta t m
  in TransactionWithMeta f

postingsWithMeta :: 
  Filename
  -> TransactionWithMeta
  -> [PostingRecord]
postingsWithMeta f (TransactionWithMeta t) =
  map (PostingRecord f) . flatten . children $ t

itemToChildren :: Filename
                  -> I.Item
                  -> Maybe [PostingRecord]
itemToChildren f i = case i of
  (I.Transaction (t, m)) -> Just (postingsWithMeta f (familyWithMeta t m))
  _ -> Nothing

itemsToChildren ::
  Filename
  -> [I.Item]
  -> [PostingRecord]
itemsToChildren f = concat . catMaybes . map (itemToChildren f)
