module Penny.Register.Cell where

import Control.Applicative
import Control.Monad
import Penny.Matcher
import Penny.Lincoln.Clatch
import Penny.Queries.Clatch
import Penny.Lincoln.Rep
import Penny.Lincoln.Qty
import Penny.Commodity
import Penny.Lincoln.Amount
import qualified Penny.Queries.Matcher as Q
import Penny.Lincoln.Field (displayScalar)
import Data.Monoid
import Data.Sequence (Seq, viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Penny.Ledger
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Sums

-- | Indicates what sort of information is in the cell.  Can be debit,
-- credit, or zero for cells that contain numeric information or
-- information directly related to numbers, such as a commodity.  May
-- be 'InfoTag' for other cells, or 'NoticeTag' if there is something
-- that should stand out.
data CellTag
  = DebitTag
  | CreditTag
  | ZeroTag
  | InfoTag
  | NoticeTag
  deriving (Eq, Ord, Show, Enum, Bounded)


qty
  :: Ledger l
  => (Commodity -> Qty -> Text)
  -> Clatch l
  -> l [(CellTag, Text)]
qty fmt clch = undefined

formatQty
  :: S3 RepNonNeutralNoSide QtyRepAnyRadix Qty
  -> Text
formatQty = undefined

labeledTree
  :: Ledger l
  => Text
  -> Clatch l
  -> l [(CellTag, Text)]
labeledTree txt clch = liftM (\x -> [(InfoTag, x)])
  $ findLabeled txt clch

findLabeled
  :: Ledger l
  => Text
  -> Clatch l
  -> l Text
findLabeled txt clch
  = renderFoundLabel clch
  . findTree
  . matchLabeledTree
  $ txt


-- | Given a Matcher that runs on a single 'TreeL', perform a
-- pre-order search for that tree.  First look in the posting metadata
-- and then in the transaction metadata.  Returns all matches.
findTree
  :: Ledger l
  => Matcher (TreeL l) l a
  -> Matcher (Clatch l) l a
findTree mtcr = matchPstg <|> matchTxn
  where
    finder = each . Q.preOrder $ mtcr
    matchTxn = do
      txn <- fmap transactionL getSubject
      ts <- txnMeta txn
      study finder ts
    matchPstg = do
      pstg <- fmap postingL getSubject
      ts <- pstgMeta pstg
      study finder ts


-- | Given some text that is the cell label, find the best tree that
-- contains that label and display it.

renderFoundLabel
  :: Ledger l
  => Clatch l
  -> Matcher (Clatch l) l Text
  -> l Text
  -- ^ Text for the cell
renderFoundLabel clch mtcr = liftM (maybe X.empty id) (observe mtcr clch)

-- | If the TreeL has a scalar, the scalar is a text field, and the
-- text field is equal to the given label, then returns the offspring
-- of that TreeL.  Does not perform any searching; this is run on the
-- current TreeL only.
matchLabeledTree
  :: Ledger l
  => Text
  -- ^ Label for the cell
  -> Matcher (TreeL l) l Text
matchLabeledTree lbl = do
  _ <- Q.scalar . Q.text . Q.equal $ lbl
  subj <- getSubject
  kids <- offspring subj
  displayForestL kids

-- | A cell with blank spaces.
spacer :: Monad m => Int -> m [(CellTag, Text)]
spacer i = return [(InfoTag, X.replicate i (X.singleton ' '))]

-- # Displaying trees

-- To deal with special Unicode characters in Emacs, use C-x 8 to
-- insert them.  C-x = will give brief information about the character
-- at point; M-x describe-char gives more detailed information.

-- | Displays a tree.  It's impractical to display any of the children
-- of the child trees, so any child tree that has children is suffixed
-- with a down arrow (which is U+2193, or ↓) to let the user know
-- something is down there.

displayTreeL
  :: Ledger l
  => TreeL l
  -> l Text
displayTreeL t = liftM2 f (scalar t) (offspring t)
  where
    f sc cs = maybe X.empty displayScalar sc <>
      if Seq.null cs then mempty else X.singleton '↓'

-- | Displays a forest of trees, with each separated by a bullet
-- (which is U+2022, or •).
displayForestL
  :: Ledger l
  => Seq (TreeL l)
  -> l Text
displayForestL sq = case viewl sq of
  EmptyL -> return X.empty
  x1 :< xs1 -> do
    t1 <- displayTreeL x1
    let dispNext t = liftM (X.cons '•') $ displayTreeL t
    liftM (F.foldl' mappend t1) $ T.mapM dispNext xs1
