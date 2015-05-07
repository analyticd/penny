{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Register report
module Penny.Register
  ( -- * Colors
    Colors(..)
  , debit
  , credit
  , neutral
  , nonLinear
  , notice
  , oddBackground
  , evenBackground

  -- * Color schemes
  , lightBackground
  , darkBackground

  -- * Column
  , Regcol

  -- * Side, commodity, and qty
  , BestField(..)
  , original
  , best
  , balance
  , side
  , Penny.Register.commodity
  , Penny.Register.qty

  -- * Forest
  , findNamedTree
  , displayForestL
  , displayTreeL
  , forest

  -- * Spacer
  , Penny.Register.spacer

  -- * Sersets
  , forward
  , backward
  , preFiltered
  , sorted
  , postFiltered
  , FileOrGlobal(..)
  , global
  , file
  , posting
  , topLine
  , Penny.Register.index
  ) where

import Control.Applicative
import Control.Lens hiding (each)
import Control.Monad
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Sequence (Seq, viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Traversable as T
import Penny.Amount
import Penny.Commodity
import Penny.Clatch
import Penny.Field (displayScalar)
import Penny.Ledger
import Penny.Natural
import Penny.Matcher (Matcher, each, getSubject, study, observe)
import Penny.Queries.Clatch
import qualified Penny.Queries.Matcher as Q
import Penny.Representation
import Penny.Serial
import Penny.Side
import Penny.Transaction
import Rainbow
import Rainbox hiding (background)
import qualified Rainbox
import Penny.Column
import Data.Sums
import Penny.Display
import qualified Data.Text as X
import Penny.Qty
import qualified Data.Map as M
import Penny.Balance
import qualified Data.Foldable as F

-- # High-level formatting

-- | Load data into this record to make a color scheme that has
-- different colors for debits and credits, with an alternating
-- background for odd- and even-numbered postings.
data Colors = Colors
  { _debit :: Radiant
  , _credit :: Radiant
  , _neutral :: Radiant
  , _nonLinear :: Radiant
  , _notice :: Radiant
  , _oddBackground :: Radiant
  , _evenBackground :: Radiant
  } deriving (Eq, Ord, Show)

makeLenses ''Colors

instance Monoid Colors where
  mempty = Colors
    { _debit = mempty
    , _credit = mempty
    , _neutral = mempty
    , _nonLinear = mempty
    , _notice = mempty
    , _oddBackground = mempty
    , _evenBackground = mempty
    }

  mappend (Colors x0 x1 x2 x3 x4 x5 x6) (Colors y0 y1 y2 y3 y4 y5 y6)
    = Colors (x0 <> y0) (x1 <> y1) (x2 <> y2) (x3 <> y3)
             (x4 <> y4) (x5 <> y5) (x6 <> y6)

-- | A single column in a register report.
type Regcol l
  = Colors
  -> (Amount -> NilOrBrimScalarAnyRadix)
  -> Column l (Clatch l)

-- | For functions that return this type, use 'original', 'best', or
-- 'balance' to get an appropriate column.
data BestField l = BestField
  { _original :: Regcol l
  -- ^ Use posting data.  Use the original, not converted, value.
  , _best :: Regcol l
  -- ^ Use posting data.  Use the converted value if available;
  -- otherwise, use the converted value.
  , _balance :: Regcol l
  -- ^ Use balance data.
  }

makeLenses ''BestField

side :: Ledger l => BestField l
side = BestField originalSide bestSide balanceSide

commodity :: Ledger l => BestField l
commodity = BestField originalCommodity
  Penny.Register.bestCommodity balanceCommodity

qty :: Ledger l => BestField l
qty = BestField originalQty Penny.Register.bestQty balanceQty

-- | Format a Qty for display.
formatQty
  :: (Amount -> NilOrBrimScalarAnyRadix)
  -- ^ Use this function for rendering a 'Qty'.
  -> S3 RepNonNeutralNoSide QtyRepAnyRadix Amount
  -> Text
formatQty rend s3 = case s3 of
  S3a rnn -> X.pack . ($ "") . display $ rnn
  S3b qrr -> X.pack . ($ "") . display
    . c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix $ qrr
  S3c amt -> X.pack . ($ "") . display . rend $ amt

linearForeground
  :: Ledger l
  => Colors
  -> Clatch l
  -> l Radiant
linearForeground clrs
  = liftM f
  . Penny.Ledger.qty
  . postingL
  where
    f q = clrs ^. case qtySide q of
      Nothing -> neutral
      Just Debit -> debit
      Just Credit -> credit

background
  :: Colors
  -> Clatch l
  -> Radiant
background clrs (Filtered (Sersetted (Serset (Forward (Serial uns)) _) _))
  | odd . naturalToInteger $ uns = clrs ^. oddBackground
  | otherwise = clrs ^. evenBackground

headerCell
  :: Colors
  -> [Text]
  -> Cell
headerCell clrs txts
  = mempty
  & rows .~ ( Seq.fromList
              . map ( Seq.singleton . fore (clrs ^. nonLinear)
                      . back (clrs ^. oddBackground) . chunk)
              $ txts)
  & Rainbox.background .~ (clrs ^. oddBackground)

doubleton :: a -> Seq (Seq a)
doubleton = Seq.singleton . Seq.singleton

convertQtyToAmount
  :: Commodity
  -> S3 a b Qty
  -> S3 a b Amount
convertQtyToAmount cy s3 = case s3 of
  S3a a -> S3a a
  S3b b -> S3b b
  S3c q -> S3c $ Amount cy q

originalQty
  :: Ledger l
  => Regcol l
originalQty clrs conv = Column header cell
  where
    header = headerCell clrs ["qty", "original"]
    cell clatch = do
      commodity <- Penny.Ledger.commodity . postingL $ clatch
      s3 <- liftM (convertQtyToAmount commodity)
        $ Penny.Queries.Clatch.originalQtyRep clatch
      singleLinearLeftTop clrs clatch (formatQty conv s3)

bestQty
  :: Ledger l
  => Regcol l
bestQty clrs conv = Column header cell
  where
    header = headerCell clrs ["qty", "best"]
    cell clatch = do
      commodity <- Penny.Queries.Clatch.bestCommodity clatch
      s3 <- liftM (convertQtyToAmount commodity)
        $ Penny.Queries.Clatch.bestQtyRep clatch
      singleLinearLeftTop clrs clatch (formatQty conv s3)

bestCommodity
  :: Ledger l
  => Regcol l
bestCommodity clrs _ = Column header cell
  where
    header = headerCell clrs ["commodity", "best"]
    cell clatch = do
      Commodity cy <- Penny.Queries.Clatch.bestCommodity clatch
      singleLinearLeftTop clrs clatch cy

originalCommodity :: Ledger l => Regcol l
originalCommodity clrs _ = Column header cell
  where
    header = headerCell clrs ["commodity", "original"]
    cell clatch = do
      Commodity cy <- Penny.Ledger.commodity . postingL $ clatch
      singleLinearLeftTop clrs clatch cy

singleLinearLeftTop
  :: Ledger l
  => Colors
  -> Clatch l
  -> Text
  -> l Cell
singleLinearLeftTop clrs clatch txt
  = liftM f (linearForeground clrs clatch)
  where
    bg = background clrs clatch
    f fg = mempty
      & rows .~ doubleton (chunk txt & fore fg & back bg)
      & vertical .~ left
      & horizontal .~ top
      & Rainbox.background .~ (background clrs clatch)

originalSide :: Ledger l => Regcol l
originalSide clrs _ = Column header cell
  where
    header = headerCell clrs ["side", "original"]
    cell clatch
      = singleLinearLeftTop clrs clatch . sideTxt
      <=< Penny.Ledger.qty . postingL
      $ clatch

bestSide :: Ledger l => Regcol l
bestSide clrs _ = Column header cell
  where
    header = headerCell clrs ["side", "best"]
    cell clatch
      = singleLinearLeftTop clrs clatch . sideTxt
      <=< Penny.Queries.Clatch.bestQty
      $ clatch

balanceCellRow
  :: Colors
  -> Clatch l
  -> Qty
  -> Text
  -> Seq (Chunk Text)
balanceCellRow clrs clatch qty = Seq.singleton . fore fg . back bg . chunk
  where
    fg = clrs ^. case qtySide qty of
      Nothing -> neutral
      Just Debit -> debit
      Just Credit -> credit
    bg = background clrs clatch

balanceSide :: Ledger l => Regcol l
balanceSide clrs _ = Column header (liftM return cell)
  where
    header = headerCell clrs ["balance", "side"]
    cell clatch
      = (\rs -> mempty & rows .~ rs
                            & Rainbox.background .~ bg
                            & vertical .~ left
                            & horizontal .~ top)
      . fmap (\(_, q) -> balanceCellRow clrs clatch q . sideTxt $ q)
      . Seq.fromList
      . M.assocs
      . (\(Balance mp) -> mp)
      . runningBalance
      $ clatch
      where
        bg = background clrs clatch


balanceCommodity :: Ledger l => Regcol l
balanceCommodity clrs _ = Column header (liftM return cell)
  where
    header = headerCell clrs ["balance", "commodity"]
    cell clatch
      = (\rs -> mempty & rows .~ rs
                       & Rainbox.background .~ bg
                       & vertical .~ left
                       & horizontal .~ top)
      . fmap (\(Commodity cy, q) -> balanceCellRow clrs clatch q cy)
      . Seq.fromList
      . M.assocs
      . (\(Balance mp) -> mp)
      . runningBalance
      $ clatch
      where
        bg = background clrs clatch


balanceQty :: Ledger l => Regcol l
balanceQty clrs conv = Column header (liftM return cell)
  where
    header = headerCell clrs ["balance", "commodity"]
    cell clatch
      = (\rs -> mempty & rows .~ rs
                       & Rainbox.background .~ bg
                       & vertical .~ left
                       & horizontal .~ top)
      . fmap (\(cy, q) -> balanceCellRow clrs clatch q
                          . X.pack . ($ "") . display
                          . conv $ Amount cy q)
      . Seq.fromList
      . M.assocs
      . (\(Balance mp) -> mp)
      . runningBalance
      $ clatch
      where
        bg = background clrs clatch

sideTxt :: Qty -> Text
sideTxt q = case qtySide q of
  Nothing -> "--"
  Just s -> X.pack . ($ "") . display $ s

--
-- # Forest
--


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
      lift $ offspring subj
    matchTxn = do
      txn <- fmap transactionL getSubject
      ts <- lift $ txnMeta txn
      study finder ts
    matchPstg = do
      pstg <- fmap postingL getSubject
      ts <- lift $ pstgMeta pstg
      study finder ts

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


displayTreeL
  :: Ledger l
  => TreeL l
  -> l Text
displayTreeL t = liftM2 f (scalar t) (offspring t)
  where
    f sc cs = maybe X.empty displayScalar sc <>
      if Seq.null cs then mempty else X.singleton '↓'

-- | Creates a 'Regcol' with the results from the given 'Matcher'.
-- The resulting 'Column' in the 'Regcol' has an empty header cell.
forest
  :: Ledger l
  => Matcher (Clatch l) l (Seq (TreeL l))
  -> Regcol l
forest mtcr clrs _ = Column mempty $ \clch -> do
  mayRes <- observe mtcr clch
  case mayRes of
    Nothing -> return mempty
    Just ts -> do
      txt <- displayForestL ts
      return $ mempty & rows .~ ( Seq.singleton
                                . Seq.singleton
                                . fore (clrs ^. nonLinear)
                                . back (background clrs clch)
                                . chunk $ txt)
                      & Rainbox.background .~ (background clrs clch)
                      & vertical .~ left
                      & horizontal .~ top

spacer :: Monad l => Int -> Regcol l
spacer i _ _ = spaces i


-- ## Sersets

-- | A column with a 'Forward' serial.  Use with the other functions
-- below, such as 'preFiltered' and 'sorted'.
forward
  :: Monad l
  => (Clatch l -> l Serset)
  -> Regcol l
forward get clrs _ = Column hdr cell
  where
    hdr = headerCell clrs ["serset", "forward"]
    cell clatch = liftM mkCell . get $ clatch
      where
        mkCell (Serset (Forward (Serial fwd)) _)
          = mempty
          & rows .~ ( Seq.singleton . Seq.singleton
                      . fore (clrs ^. nonLinear)
                      . back (background clrs clatch)
                      . chunk . X.pack . show . naturalToInteger
                      $ fwd )
          & vertical .~ left
          & horizontal .~ top
          & Rainbox.background .~ (background clrs clatch)


-- | A column with a 'Backward' serial.  Use with the other functions
-- below, such as 'preFiltered' and 'sorted'.
backward
  :: Monad l
  => (Clatch l -> l Serset)
  -> Regcol l
backward get clrs _ = Column hdr cell
  where
    hdr = headerCell clrs ["serset", "backward"]
    cell clatch = liftM mkCell . get $ clatch
      where
        mkCell (Serset _ (Backward (Serial rev)))
          = mempty
          & rows .~ ( Seq.singleton . Seq.singleton
                      . fore (clrs ^. nonLinear)
                      . back (background clrs clatch)
                      . chunk . X.pack . show . naturalToInteger
                      $ rev )
          & vertical .~ left
          & horizontal .~ top
          & Rainbox.background .~ (background clrs clatch)


-- | Use with 'forward' and 'backward', for instance:
--
-- @
-- 'forward' 'preFiltered'
-- @
preFiltered :: Monad l => Clatch l -> l Serset
preFiltered = fmap return sersetPreFiltered


-- | Use with 'forward' and 'backward', for instance:
--
-- @
-- 'backward' 'sorted'
-- @
sorted :: Monad l => Clatch l -> l Serset
sorted = fmap return sersetSorted

-- | Use with 'forward' and 'backward', for instance:
--
-- @
-- 'forward' 'postFiltered'
-- @
postFiltered :: Monad l => Clatch l -> l Serset
postFiltered = fmap return sersetPostFiltered


-- | For functions that return values of this type, use 'global' or
-- 'file' to get an appropriate column.
data FileOrGlobal l = FileOrGlobal
  { _global :: Clatch l -> l Serset
  -- ^ Use the global 'Serset'.
  , _file :: Clatch l -> l Serset
  -- ^ Use the file 'Serset'.
  }

makeLenses ''FileOrGlobal

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


lightBackground :: Colors
lightBackground = Colors
  { _debit = blue
  , _credit = magenta
  , _neutral = black
  , _nonLinear = black
  , _notice = red
  , _oddBackground = mempty
  , _evenBackground = white <> color256 230
  -- 230: pale yellow
  }

darkBackground :: Colors
darkBackground = Colors
  { _debit = blue
  , _credit = magenta
  , _neutral = white
  , _nonLinear = white
  , _notice = red
  , _oddBackground = mempty
  , _evenBackground = black <> color256 237
  -- 237: grey
  }
