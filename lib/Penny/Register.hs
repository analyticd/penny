{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | The Register report
module Penny.Register where

{-
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

  -- * Register Report
  , Register(..)
  , showHeaders
  , colors
  , columns

  -- * Convenient sets of columns
  , (|+>)
  , (<+|)
  , (<+>)
  , amount
  , balances
  , datePayeeAccount
  , register
  ) where
-}

import Control.Lens hiding (each)
import Control.Monad
import Data.Monoid
import Data.Sequence (Seq, viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Traversable as T
import Penny.Amount
import Penny.Commodity
import Penny.Converted
import Penny.Clatch
import Penny.Clatch.Shortcut (date, payee, account)
import qualified Penny.Clatch.Shortcut
import Penny.Field (displayScalar)
import Penny.Ledger (Ledger, TreeL)
import qualified Penny.Ledger
import Penny.Natural
import Penny.Report
import Penny.Representation
import Penny.Serial
import Penny.Side
import Penny.Transaction
import Penny.Transbox
import Penny.Viewpost
import Penny.SeqUtil
import Rainbow
import Rainbox hiding (background, spacer)
import qualified Rainbox
import Penny.Column
import Data.Sums
import Penny.Display
import qualified Data.Text as X
import Penny.Qty
import qualified Data.Map as M
import Penny.Balance
import qualified Data.Foldable as F
import Penny.ListT

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

-- # Cell makers

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

background
  :: Colors
  -> Clatch l
  -> Radiant
background clrs clch
  | odd serial = clrs ^. oddBackground
  | otherwise  = clrs ^. evenBackground
  where
    serial = clch ^.
      transboxee.viewpostee.convertee.sersetee.sersetee
        .runningBalancee.serset.Penny.Serial.forward.to naturalToInteger

linearForeground
  :: Ledger l
  => Colors
  -> Clatch l
  -> l Radiant
linearForeground clrs
  = liftM f
  . Penny.Ledger.qty
  . view (transboxee.viewpost.onView)
  where
    f q = clrs ^. case qtySide q of
      Nothing -> neutral
      Just Debit -> debit
      Just Credit -> credit

-- # Side

sideTxt :: Qty -> Text
sideTxt q = case qtySide q of
  Nothing -> "--"
  Just s -> X.pack . ($ "") . display $ s

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
      <=< Penny.Ledger.qty . view (transboxee.viewpost.onView)
      $ clatch

bestSide :: Ledger l => Regcol l
bestSide clrs _ = Column header cell
  where
    header = headerCell clrs ["side", "best"]
    cell clatch
      = singleLinearLeftTop clrs clatch . sideTxt
      <=< return . view Penny.Amount.qty
      <=< bestAmount . view transboxee
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
      . view (transboxee.viewpostee.convertee.sersetee
              .sersetee.runningBalance)
      $ clatch
      where
        bg = background clrs clatch


side :: Ledger l => BestField l
side = BestField originalSide bestSide balanceSide

-- # Commodity

originalCommodity :: Ledger l => Regcol l
originalCommodity clrs _ = Column header cell
  where
    header = headerCell clrs ["commodity", "original"]
    cell clatch = do
      Commodity cy <- Penny.Ledger.commodity
        (clatch ^. transboxee . viewpost . onView)
      singleLinearLeftTop clrs clatch cy


bestCommodity
  :: Ledger l
  => Regcol l
bestCommodity clrs _ = Column header cell
  where
    header = headerCell clrs ["commodity", "best"]
    cell clatch = do
      Amount (Commodity cy) _ <- bestAmount (clatch ^. transboxee)
      singleLinearLeftTop clrs clatch cy


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
      . view (transboxee.viewpostee.convertee.sersetee
              .sersetee.runningBalance)
      $ clatch
      where
        bg = background clrs clatch


commodity :: Ledger l => BestField l
commodity = BestField originalCommodity
  Penny.Register.bestCommodity balanceCommodity

-- # Qty

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
      commodity <- Penny.Ledger.commodity . view (transboxee.viewpost.onView)
        $ clatch
      s3 <- liftM (convertQtyToAmount commodity)
        . Penny.Ledger.originalQtyRep
        . view (transboxee.viewpost.onView)
        $ clatch
      singleLinearLeftTop clrs clatch (formatQty conv s3)

bestQty
  :: Ledger l
  => Regcol l
bestQty clrs conv = Column header cell
  where
    header = headerCell clrs ["qty", "best"]
    cell clatch = do
      Amount commodity _ <- bestAmount (clatch ^. transboxee)
      s3 <- liftM (convertQtyToAmount commodity)
        $ bestQtyRep (clatch ^. transboxee)
      singleLinearLeftTop clrs clatch (formatQty conv s3)


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
      . view (transboxee.viewpostee.convertee.sersetee
              .sersetee.runningBalance)
      $ clatch
      where
        bg = background clrs clatch

qty :: Ledger l => BestField l
qty = BestField originalQty Penny.Register.bestQty balanceQty

--
-- # Forest
--


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
displayTreeL t = liftM2 f (Penny.Ledger.scalar t) (Penny.Ledger.offspring t)
  where
    f sc cs = maybe X.empty displayScalar sc <>
      if Seq.null cs then mempty else X.singleton '↓'

formatForestRow
  :: Colors
  -> Radiant
  -> Text
  -> Chunk Text
formatForestRow clrs bk
  = fore (clrs ^. nonLinear)
  . back bk
  . chunk

formatForestCell
  :: Radiant
  -> Seq (Chunk Text)
  -> Cell
formatForestCell bk rws = mempty
  & rows .~ fmap Seq.singleton rws
  & Rainbox.background .~ bk
  & vertical .~ left
  & horizontal .~ top

forestRow
  :: Ledger l
  => Colors
  -> Radiant
  -> Seq (TreeL l)
  -> l (Chunk Text)
forestRow clrs bk = liftM (formatForestRow clrs bk) . displayForestL

forestCell
  :: Ledger l
  => Clatch l
  -> Colors
  -> Seq (Seq (TreeL l))
  -> l Cell
forestCell clch clrs
  = liftM (formatForestCell bk)
  . T.mapM (forestRow clrs bk)
  where
    bk = background clrs clch

-- | Creates a 'Regcol' with the results from the given matcher.
-- The resulting 'Column' in the 'Regcol' has an empty header cell.
forest
  :: Ledger l
  => (Clatch l -> ListT l (Seq (TreeL l)))
  -> Regcol l
forest mtcr clrs _ = Column mempty $ \clch ->
  observeAll (mtcr clch) >>= forestCell clch clrs

-- # Spacer

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
        mkCell (Serset fwd _)
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
        mkCell (Serset _ rev)
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
preFiltered = return . view (transboxee.viewpostee.convertee.serset)


-- | Use with 'forward' and 'backward', for instance:
--
-- @
-- 'backward' 'sorted'
-- @
sorted :: Monad l => Clatch l -> l Serset
sorted = return . view (transboxee.viewpostee.convertee.sersetee.serset)

-- | Use with 'forward' and 'backward', for instance:
--
-- @
-- 'forward' 'postFiltered'
-- @
postFiltered :: Monad l => Clatch l -> l Serset
postFiltered = return . view
  (transboxee.viewpostee.convertee.sersetee.sersetee.runningBalancee.serset)


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
      PostingSer _ (GlobalSer g) _ <- Penny.Ledger.postingSer
        (clch ^. (transboxee.viewpost.onView))
      return g
    fle clch = do
      PostingSer (FileSer f) _ _ <- Penny.Ledger.postingSer
        (clch ^. (transboxee.viewpost.onView))
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
      TopLineSer _ (GlobalSer g) <- Penny.Ledger.topLineSer
        (clch ^. Penny.Transbox.transaction)
      return g
    fle clch = do
      TopLineSer (FileSer f) _ <- Penny.Ledger.topLineSer
        (clch ^. Penny.Transbox.transaction)
      return f

-- | Use with 'forward' and 'backward', for instance:
--
-- @
-- 'backward' 'index'
-- @
index :: Ledger l => Clatch l -> l Serset
index clch = do
  PostingSer _ _ (PostingIndex s) <- Penny.Ledger.postingSer
    (clch ^. transboxee.viewpost.onView)
  return s


-- # Colors

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

--
-- # Register
--

data Register l = Register
  { _showHeaders :: Bool
  , _colors :: Colors
  , _columns :: Seq (Regcol l)
  }

makeLenses ''Register

-- | For '_showHeaders', 'mempty' is 'False' and 'mappend' is '||'.
-- For the other two fields, the underlying 'Monoid' instance is used.
instance Monoid (Register l) where
  mempty = Register False mempty mempty
  mappend (Register x0 x1 x2) (Register y0 y1 y2)
    = Register (x0 || y0) (x1 <> y1) (x2 <> y2)

instance Report Register where
  printReport (Register showHeaders colors columns) formatter sq = table cols sq
    where
      cols = fmap modifyHeader . fmap (\f -> f colors formatter) $ columns
      modifyHeader
        | showHeaders = id
        | otherwise = header .~ mempty

--
-- # Default reports
--


-- | Appends a single column to the end; adds a spacer if the list of
-- columns is not empty.
(<+|) :: Monad a => Regcol a -> Seq (Regcol a) -> Seq (Regcol a)
r <+| sq
  | Seq.null sq = Seq.singleton r
  | otherwise = r <| spacer 1 <| sq

infixr 5 <+|

-- | Appends a single column to the end; adds a spacer if the list of
-- columns is not empty.
(|+>) :: Monad a => Seq (Regcol a) -> Regcol a -> Seq (Regcol a)
sq |+> r
  | Seq.null sq = Seq.singleton r
  | otherwise = sq |> spacer 1 |> r

infixl 5 |+>

-- | Puts two lists of columns together; adds a spacer if both lists
-- are not empty.
(<+>) :: Monad a => Seq (Regcol a) -> Seq (Regcol a) -> Seq (Regcol a)
l <+> r
  | not (Seq.null l) && not (Seq.null r) = l <> Seq.singleton (spacer 1) <> r
  | otherwise = l <> r

infixr 6 <+>

-- | The amount for this posting.
amount :: Ledger l => Seq (Regcol l)
amount = mempty
  |+> side ^. best
  |+> Penny.Register.commodity ^. best
  |+> Penny.Register.qty ^. best

-- | Balances for this posting.
balances :: Ledger l => Seq (Regcol l)
balances = mempty
  |+> side ^. balance
  |+> Penny.Register.commodity ^. best
  |+> Penny.Register.qty ^. best

{-
-- | The date, payee, and account fields.
datePayeeAccount :: Ledger l => Seq (Regcol l)
datePayeeAccount = mempty
  |+> forest (Penny.Clatch.Shortcut.posting date)
  |+> forest (Penny.Clatch.Shortcut.posting payee)
  |+> forest (Penny.Clatch.Shortcut.posting account)
-}

{-
-- | A default register report.  Shows the date, payee, account,
-- amount, and balances.
register :: Ledger l => Seq (Regcol l)
register = datePayeeAccount <+> amount <+> balances
-}
