{-# LANGUAGE OverloadedStrings #-}
module Penny.Register.Cell where

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence (Seq, viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Sums
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Traversable as T
import Penny.Amount
import Penny.Clatch
import Penny.Commodity
import Penny.Display
import Penny.Field (displayScalar, Realm(..))
import Penny.Ledger
import Penny.Matcher
import Penny.Natural
import Penny.Representation
import Penny.Qty
import Penny.Queries.Clatch
import Penny.Serial
import Penny.Side
import Penny.Transaction
import qualified Penny.Queries.Matcher as Q

-- | Indicates what sort of information is in the cell.  Can be debit,
-- credit, or zero for cells that contain numeric information or
-- information directly related to numbers, such as a commodity.  May
-- be 'InfoTag' for other cells, or 'NoticeTag' if there is something
-- that should stand out.
data CellTag
  = Linear (Maybe Side)
  | NonLinear
  | Notice
  deriving (Eq, Ord, Show)

-- # Cells

qty
  :: Ledger l
  => BestField l (Amount -> RepNonNeutralNoSide)
qty = BestField originalQty Penny.Register.Cell.bestQty

commodity
  :: Ledger l
  => BestField l a
commodity = BestField originalCommodity
  Penny.Register.Cell.bestCommodity

side
  :: Ledger l
  => BestField l a
side = BestField originalSide bestSide

-- | Creates a cell by attempting to find a tree in the posting
-- metadata whose scalar exactly matches the given name.  If no tree
-- is found in the posting metadata, looks in the transaction
-- metadata.  If no tree is found there, the cell will be empty.
labeledTree
  :: Ledger l
  => Text
  -> a
  -> Clatch l
  -> l [(CellTag, Text)]
labeledTree txt _ clch = liftM (\x -> [(NonLinear, x)])
  $ findLabeled txt clch

-- | A cell with the given number of blank spaces.
spacer :: Monad m => Int -> a -> b -> m [(CellTag, Text)]
spacer i _ _ = return [(NonLinear, X.replicate i (X.singleton ' '))]

-- ## Sersets

-- Use these with 'forward' and 'reverse' to get the serial you want.

preFiltered :: Monad l => Clatch l -> l Serset
preFiltered = fmap return sersetPreFiltered

sorted :: Monad l => Clatch l -> l Serset
sorted = fmap return sersetSorted

postFiltered :: Monad l => Clatch l -> l Serset
postFiltered = fmap return sersetPostFiltered

-- Gets the Sersets from a posting or the transaction.  Use with
-- 'global' and 'file'.

posting :: Ledger l => Clatch l -> FileOrGlobal l
posting clch = FileOrGlobal glbl fle
  where
    glbl = do
      PostingSer _ g _ <- postingSer . postingL $ clch
      return g
    fle = do
      PostingSer f _ _ <- postingSer . postingL $ clch
      return f

topLine :: Ledger l => Clatch l -> FileOrGlobal l
topLine clch = FileOrGlobal glbl fle
  where
    glbl = do
      TopLineSer _ g <- topLineSer . transactionL $ clch
      return g
    fle = do
      TopLineSer f _ <- topLineSer . transactionL $ clch
      return f

index :: Ledger l => Clatch l -> l Serset
index clch = do
  PostingSer _ _ (PostingIndex s) <- postingSer . postingL $ clch
  return s


-- # Helpers

data BestField l a = BestField
  { original :: (a -> Clatch l -> l [(CellTag, Text)])
  , best :: (a -> Clatch l -> l [(CellTag, Text)])
  }

data FileOrGlobal l = FileOrGlobal
  { global :: l GlobalSer
  , file :: l FileSer
  }

-- | Displays the 'Qty' only.  Uses the converted 'Qty' if there is
-- one.
bestQty
  :: Ledger l
  => (Amount -> RepNonNeutralNoSide)
  -> Clatch l
  -> l [(CellTag, Text)]
bestQty fmt clch = do
  cy <- Penny.Queries.Clatch.bestCommodity clch
  s3 <- liftM (convertQtyToAmount cy)
    $ Penny.Queries.Clatch.bestQtyRep clch
  tag <- linearTag clch
  return [(tag, formatQty fmt s3)]

-- | Displays the 'Qty' only.  Always uses the original, not
-- converted, q'Qty'.
originalQty
  :: Ledger l
  => (Amount -> RepNonNeutralNoSide)
  -> Clatch l
  -> l [(CellTag, Text)]
originalQty fmt clch = do
  cy <- Penny.Ledger.commodity . postingL $ clch
  s3 <- liftM (convertQtyToAmount cy)
    $ Penny.Queries.Clatch.originalQtyRep clch
  tag <- linearTag clch
  return [(tag, formatQty fmt s3)]

-- | Displays the 'Commodity' only.  Uses the converted 'Commodity' if
-- there is one.
bestCommodity
  :: Ledger l
  => a
  -> Clatch l
  -> l [(CellTag, Text)]
bestCommodity _ clch = do
  tag <- linearTag clch
  Commodity cy <- Penny.Queries.Clatch.bestCommodity clch
  return [(tag, cy)]

-- | Displays the 'Commodity' only.  Always uses the original
-- 'Commodity'.
originalCommodity
  :: Ledger l
  => a
  -> Clatch l
  -> l [(CellTag, Text)]
originalCommodity _ clch = do
  tag <- linearTag clch
  Commodity cy <- Penny.Ledger.commodity . postingL $ clch
  return [(tag, cy)]

-- | Displays the 'Side' only.  Uses the converted 'Amount' if there
-- is one.
bestSide
  :: Ledger l
  => a
  -> Clatch l
  -> l [(CellTag, Text)]
bestSide _
  = liftM sideCell
  . Penny.Queries.Clatch.bestQty


-- | Displays the 'Side' only.  Always uses the original 'Amount'.
originalSide
  :: Ledger l
  => a
  -> Clatch l
  -> l [(CellTag, Text)]
originalSide _
  = liftM sideCell
  . Penny.Ledger.qty
  . postingL


sideCell
  :: Qty
  -> [(CellTag, Text)]
sideCell q = [(Linear sd, txt)]
  where
    sd = qtySide q
    txt = case sd of
      Nothing -> "--"
      Just s -> X.pack . ($ "") . display $ s

linearTag
  :: Ledger l
  => Clatch l
  -> l CellTag
linearTag = liftM (Linear . qtySide)
  . Penny.Ledger.qty
  . postingL

convertQtyToAmount
  :: Commodity
  -> S3 a b Qty
  -> S3 a b Amount
convertQtyToAmount cy s3 = case s3 of
  S3a a -> S3a a
  S3b b -> S3b b
  S3c q -> S3c $ Amount cy q

-- | Format a Qty for display.
formatQty
  :: (Amount -> RepNonNeutralNoSide)
  -- ^ Use this function for rendering a 'Qty'.
  -> S3 RepNonNeutralNoSide QtyRepAnyRadix Amount
  -> Text
formatQty rend s3 = case s3 of
  S3a rnn -> X.pack . ($ "") . display $ rnn
  S3b qrr -> X.pack . ($ "") . display
    . c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix $ qrr
  S3c amt -> X.pack . ($ "") . display . rend $ amt

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


forward
  :: Monad l
  => (Clatch l -> l Serset)
  -> a
  -> Clatch l
  -> l [(CellTag, Text)]
forward get _ clch = liftM mkCell (get clch)
  where
    mkCell (Serset (Forward (Serial fwd)) _) =
      [(NonLinear, X.pack . show . naturalToInteger $ fwd)]

reverse
  :: Monad l
  => (Clatch l -> l Serset)
  -> a
  -> Clatch l
  -> l [(CellTag, Text)]
reverse get _ clch = liftM mkCell (get clch)
  where
    mkCell (Serset _ (Reverse (Serial rev))) =
      [(NonLinear, X.pack . show . naturalToInteger $ rev)]

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
