{-# LANGUAGE OverloadedStrings #-}
module Penny.Register.Cell
  ( CellTag(..)
  -- * Quantities, commodities, and sides
  --
  -- Use 'original', 'best', or 'balance' to determine whether to use
  -- the converted posting data (if available), the original posting
  -- data, or the balance data.
  --
  -- @
  -- -- Show the converted quantity, if there is one; otherwise
  -- -- show the original quantity
  -- let qtyColumn = best qty
  --
  -- -- Show the running balance
  -- let balQtyColumn = balance qty
  -- @
  , BestField(..)
  , Penny.Register.Cell.qty
  , Penny.Register.Cell.commodity
  , side

  -- * Cells from forests
  , forest
  , findNamedTree

  -- * White space
  , spacer

  -- * Serials
  , FileOrGlobal(..)
  , forward
  , backward
  , preFiltered
  , sorted
  , postFiltered
  , posting
  , topLine
  , Penny.Register.Cell.index
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid
import Data.Sequence (Seq, viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Sums
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Traversable as T
import Penny.Amount
import Penny.Balance
import Penny.Clatch
import Penny.Commodity
import Penny.Display
import Penny.Field (displayScalar)
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
qty = BestField originalQty Penny.Register.Cell.bestQty balanceQty

commodity
  :: Ledger l
  => BestField l a
commodity = BestField originalCommodity
  Penny.Register.Cell.bestCommodity balanceCommodity

side
  :: Ledger l
  => BestField l a
side = BestField originalSide bestSide balanceSide


-- | Creates a Cell that runs the given Matcher on the Clatch and puts
-- the resulting Forest into a cell.
forest
  :: Ledger l
  => Matcher (Clatch l) l (Seq (TreeL l))
  -> a
  -> Clatch l
  -> l [(CellTag, Text)]
forest mtcr _ clch = do
  mayRes <- observe mtcr clch
  case mayRes of
    Nothing -> return [(NonLinear, X.empty)]
    Just ts -> do
      txt <- displayForestL ts
      return [(NonLinear, txt)]

-- | Creates a 'Matcher' that looks for a parent tree with the exact
-- name given.  First performs a pre-order search in the metadata of
-- the posting; then performs a pre-order search in the metadata for
-- the top line.  If successful, returns the child forest.
--
-- Use with 'forest'; for example:
--
-- @
-- > :set -XOverloadedStrings
-- > let column = 'forest' $ 'findNamedTree' \"acct\"
-- @
findNamedTree
  :: Ledger l
  => Text
  -> Matcher (Clatch l) l (Seq (TreeL l))
findNamedTree txt = matchPstg <|> matchTxn
  where
    finder = each . Q.preOrder $ mtcr
    mtcr = do
      _ <- Q.scalar . Q.text . Q.equal $ txt
      subj <- getSubject
      offspring subj
    matchTxn = do
      txn <- fmap transactionL getSubject
      ts <- txnMeta txn
      study finder ts
    matchPstg = do
      pstg <- fmap postingL getSubject
      ts <- pstgMeta pstg
      study finder ts

-- | A cell with the given number of blank spaces.
spacer :: Monad m => Int -> a -> b -> m [(CellTag, Text)]
spacer i _ _ = return [(NonLinear, X.replicate i (X.singleton ' '))]

-- ## Sersets

-- | A column with a 'Forward' serial.  Use with the other functions
-- below, such as 'preFiltered' and 'sorted'.
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

-- | A column with a 'Backward' serial.  Use with the other functions
-- below, such as 'preFiltered' and 'sorted'.
backward
  :: Monad l
  => (Clatch l -> l Serset)
  -> a
  -> Clatch l
  -> l [(CellTag, Text)]
backward get _ clch = liftM mkCell (get clch)
  where
    mkCell (Serset _ (Backward (Serial rev))) =
      [(NonLinear, X.pack . show . naturalToInteger $ rev)]

-- Use these with 'forward' and 'backward' to get the serial you want.

-- | Use with 'forward and backward', for instance:
--
-- @
-- 'forward' 'preFiltered'
-- @
preFiltered :: Monad l => Clatch l -> l Serset
preFiltered = fmap return sersetPreFiltered


-- | Use with 'forward and backward', for instance:
--
-- @
-- 'backward' 'sorted'
-- @
sorted :: Monad l => Clatch l -> l Serset
sorted = fmap return sersetSorted

-- | Use with 'forward and backward', for instance:
--
-- @
-- 'forward' 'postFiltered'
-- @
postFiltered :: Monad l => Clatch l -> l Serset
postFiltered = fmap return sersetPostFiltered

-- Gets the Sersets from a posting or the transaction.  Use with
-- 'global' and 'file'.

-- | Use with 'forward', 'backward', 'file', and 'global', for
-- instance:
--
-- @
-- 'forward' $ 'global' 'posting'
-- @
posting :: Ledger l => FileOrGlobal l
posting = FileOrGlobal glbl fle
  where
    glbl clch = do
      PostingSer _ (GlobalSer g) _ <- postingSer . postingL $ clch
      return g
    fle clch = do
      PostingSer (FileSer f) _ _ <- postingSer . postingL $ clch
      return f

-- | Use with 'forward', 'backward', 'file', and 'global', for
-- instance:
--
-- @
-- 'forward' $ 'global' 'topLine'
-- @
topLine :: Ledger l => FileOrGlobal l
topLine = FileOrGlobal glbl fle
  where
    glbl clch = do
      TopLineSer _ (GlobalSer g) <- topLineSer . transactionL $ clch
      return g
    fle clch = do
      TopLineSer (FileSer f) _ <- topLineSer . transactionL $ clch
      return f

-- | Use with 'forward' and 'backward', for instance:
--
-- @
-- 'backward' 'index'
-- @
index :: Ledger l => Clatch l -> l Serset
index clch = do
  PostingSer _ _ (PostingIndex s) <- postingSer . postingL $ clch
  return s


-- # Helpers

-- | For functions that return this type, use 'original' or 'best' to
-- get an appropriate column.
data BestField l a = BestField
  { original :: (a -> Clatch l -> l [(CellTag, Text)])
  -- ^ Use posting data.  Use the original, not converted, value.
  , best :: (a -> Clatch l -> l [(CellTag, Text)])
  -- ^ Use posting data.  Use the converted value if available;
  -- otherwise, use the converted value.
  , balance :: (a -> Clatch l -> l [(CellTag, Text)])
  -- ^ Use balance data.
  }

-- | For functions that return values of this type, use 'global' or
-- 'file' to get an appropriate column.
data FileOrGlobal l = FileOrGlobal
  { global :: Clatch l -> l Serset
  -- ^ Use the global 'Serset'.
  , file :: Clatch l -> l Serset
  -- ^ Use the file 'Serset'.
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
  = liftM ((:[]) . sideCell)
  . Penny.Queries.Clatch.bestQty


-- | Displays the 'Side' only.  Always uses the original 'Amount'.
originalSide
  :: Ledger l
  => a
  -> Clatch l
  -> l [(CellTag, Text)]
originalSide _
  = liftM ((:[]) . sideCell)
  . Penny.Ledger.qty
  . postingL


sideCell
  :: Qty
  -> (CellTag, Text)
sideCell q = (Linear sd, txt)
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

-- # Balances

balanceQty
  :: Monad m
  => (Amount -> RepNonNeutralNoSide)
  -> Clatch m
  -> m [(CellTag, Text)]
balanceQty conv
  = return
  . fmap qtyLine
  . M.assocs
  . (\(Balance mp) -> mp)
  . runningBalance
  where
    qtyLine (cy, qt) = (Linear . qtySide $ qt, txt)
      where
        txt = X.pack . ($ "") . display
          . conv $ Amount cy qt

balanceCommodity
  :: Monad m
  => a
  -> Clatch m
  -> m [(CellTag, Text)]
balanceCommodity _
  = return
  . fmap commodityLine
  . M.assocs
  . (\(Balance mp) -> mp)
  . runningBalance
  where
    commodityLine (Commodity x, qt) = (Linear . qtySide $ qt, x)

balanceSide
  :: Monad m
  => a
  -> Clatch m
  -> m [(CellTag, Text)]
balanceSide _
  = return
  . fmap (sideCell . snd)
  . M.assocs
  . (\(Balance mp) -> mp)
  . runningBalance


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
