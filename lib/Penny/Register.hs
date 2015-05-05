{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Register report
module Penny.Register where

import Control.Applicative
import Control.Lens hiding (each)
import Control.Monad
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Traversable as T
import Penny.Amount
import Penny.Clatch
import Penny.Ledger
import Penny.Natural
import Penny.Matcher (Matcher, each, getSubject, study)
import Penny.Queries.Clatch
import qualified Penny.Queries.Matcher as Q
import Penny.Representation
import Penny.Serial
import Penny.Side
import Rainbow
import Rainbox (Box, Alignment, Vertical, tableByRows, center, Cell(..), top,
  left)
import Penny.Column
import Data.Sums
import Penny.Display
import qualified Data.Text as X
import Penny.Side
import Penny.Qty

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

data ColumnInput l = ColumnInput
  { _formatAmount :: Amount -> NilOrBrimScalarAnyRadix
  , _clatch :: Clatch l
  , _colors :: Colors
  }

makeLenses ''ColumnInput


-- | For functions that return this type, use 'original', 'best', or
-- 'balance' to get an appropriate column.
data BestField l = BestField
  { original :: Column l (ColumnInput l)
  -- ^ Use posting data.  Use the original, not converted, value.
  , best :: Column l (ColumnInput l)
  -- ^ Use posting data.  Use the converted value if available;
  -- otherwise, use the converted value.
  , balance :: Column l (ColumnInput l)
  -- ^ Use balance data.
  }

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
headerCell clrs txts = Cell
  { _rows
      = Seq.fromList
      . map Seq.singleton
      . map (fore (clrs ^. nonLinear) . back (clrs ^. oddBackground))
      . map chunk
      $ txts
  , _horizontal = top
  , _vertical = left
  , _background = clrs ^. oddBackground
  }

originalQty
  :: Ledger l
  => Column l (ColumnInput l)
originalQty = undefined


{-
  ( -- * Columns
    MakeLines
  , Column(..)
  , Columns(..)

  -- * Balance-related fields
  , BestField
  , original
  , best
  , balance

  , I.qty
  , I.commodity
  , I.side

  -- * Tree-related fields
  , findNamedTree
  , forest

  -- * Spacers
  , spacer

  -- * Sersets
  , forward
  , backward
  , preFiltered
  , sorted
  , postFiltered
  , I.FileOrGlobal(..)
  , I.posting
  , I.topLine
  , I.index

  -- * Low-level formatting
  , LineTag(..)
  , CellFormatter(..)
  , CellFormatterFromClatch(..)
  , makeCell
  , makeRegisterReport
  , noColors

  -- * High-level formatting
  , Colors(..)
  , alternating
  , darkBackground
  , lightBackground
  ) where

import Penny.Register.Individual
  ( LineTag(..), BestField )
import qualified Penny.Register.Individual as I

type MakeLines l
  = (Amount -> NilOrBrimScalarAnyRadix)
  -> Clatch l
  -> l (Seq (LineTag, Text))

data Column l = Column (Alignment Vertical) (MakeLines l)

newtype Columns l = Columns (Seq (Column l))

instance Monoid (Columns l) where
  mempty = Columns Seq.empty
  mappend (Columns x) (Columns y) = Columns $ x <> y

original
  :: Alignment Vertical
  -> BestField l (Amount -> NilOrBrimScalarAnyRadix)
  -> Columns l
original align = Columns . Seq.singleton . Column align . I.original

best
  :: Alignment Vertical
  -> BestField l (Amount -> NilOrBrimScalarAnyRadix)
  -> Columns l
best align = Columns . Seq.singleton . Column align . I.best

balance
  :: Alignment Vertical
  -> BestField l (Amount -> NilOrBrimScalarAnyRadix)
  -> Columns l
balance align = Columns . Seq.singleton . Column align . I.balance

-- | Creates a Cell that runs the given Matcher on the Clatch and puts
-- the resulting Forest into a cell.
forest
  :: Ledger l
  => Alignment Vertical
  -> Matcher (Clatch l) l (Seq (TreeL l))
  -> Columns l
forest align = Columns . Seq.singleton . Column align . I.forest


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

-- # Spacers

spacer
  :: Monad m
  => Int
  -> Columns m
spacer = Columns . Seq.singleton . Column center . I.spacer

-- # Sersets

-- | A column with a 'Forward' serial.  Use with the other functions
-- below, such as 'preFiltered' and 'sorted'.
forward
  :: Monad l
  => Alignment Vertical
  -> (Clatch l -> l Serset)
  -> Columns l
forward align = Columns . Seq.singleton . Column align . I.forward

-- | A column with a 'Backward' serial.  Use with the other functions
-- below, such as 'preFiltered' and 'sorted'.
backward
  :: Monad l
  => Alignment Vertical
  -> (Clatch l -> l Serset)
  -> Columns l
backward align = Columns . Seq.singleton . Column align . I.backward


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

-- # Low-level formatting

data CellFormatter
  = CellFormatter Radiant (Amount -> NilOrBrimScalarAnyRadix)
                  (LineTag -> Scheme)

newtype CellFormatterFromClatch l
  = CellFormatterFromClatch (Clatch l -> CellFormatter)

makeRegisterReport
  :: Ledger l
  => CellFormatterFromClatch l
  -> Columns l
  -> Seq (Clatch l)
  -> l (Box Vertical)
makeRegisterReport formatter cols
  = liftM tableByRows
  . makeRows (makeRow (makeCell formatter) cols)


makeCell
  :: Ledger l
  => CellFormatterFromClatch l
  -> Clatch l
  -> Column l
  -> l Cell
makeCell (CellFormatterFromClatch getTriple) cltch (Column align mkLines)
  = liftM f $ mkLines converter cltch
  where
    CellFormatter bkgnd converter getScheme = getTriple cltch
    mkRow (tag, txt) = Seq.singleton $ Chunk (getScheme tag) txt
    f sqLineTagsAndText = Cell cks top align bkgnd
      where
        cks = fmap mkRow $ sqLineTagsAndText

makeRow
  :: Ledger l
  => (Clatch l -> Column l -> l Cell)
  -- ^ Use 'makeCell'
  -> Columns l
  -> Clatch l
  -> l (Seq Cell)
makeRow f (Columns cols) cltch = T.mapM (f cltch) cols

makeRows
  :: Ledger l
  => (Clatch l -> l (Seq Cell))
  -- ^ Use makeRow
  -> Seq (Clatch l)
  -> l (Seq (Seq Cell))
makeRows make sq = T.mapM make sq


-- | A basic format with no colors.
noColors :: (Amount -> NilOrBrimScalarAnyRadix) -> CellFormatterFromClatch l
noColors converter = CellFormatterFromClatch $ \_ ->
  CellFormatter mempty converter (const mempty)


alternating
  :: Colors
  -> (Clatch l -> Amount -> NilOrBrimScalarAnyRadix)
  -> CellFormatterFromClatch l
alternating colors converter = CellFormatterFromClatch f
  where
    f clatch = CellFormatter background (converter clatch) formatter
      where
        (Serset (Forward (Serial fwd)) _) = sersetPostFiltered clatch
        background = if even $ naturalToInteger fwd
          then evenBackground colors
          else oddBackground colors
        formatter lineTag = foregroundSchemeFromRadiant $ case lineTag of
          Linear Nothing -> neutral colors
          Linear (Just Debit) -> debit colors
          Linear (Just Credit) -> credit colors
          NonLinear -> nonLinear colors
          Notice -> notice colors

foregroundSchemeFromRadiant :: Radiant -> Scheme
foregroundSchemeFromRadiant (Radiant c8 c256) = mempty
  & style8 . fore .~ c8
  & style256 . fore .~ c256

lightBackground :: Colors
lightBackground = Colors
  { debit = R.blue
  , credit = R.magenta
  , neutral = R.black
  , nonLinear = R.black
  , notice = R.red
  , oddBackground = mempty
  , evenBackground = Radiant (Color $ Just white) (Color $ Just 230)
  -- 230: pale yellow
  }

darkBackground :: Colors
darkBackground = Colors
  { debit = R.blue
  , credit = R.magenta
  , neutral = R.white
  , nonLinear = R.white
  , notice = R.red
  , oddBackground = mempty
  , evenBackground = Radiant (Color $ Just black) (Color $ Just 237)
  -- 237: grey
  }
-}
