{-# LANGUAGE OverloadedStrings #-}
-- | The Register report
module Penny.Register
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

import Control.Applicative
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
import Penny.Register.Individual
  ( LineTag(..), BestField )
import qualified Penny.Register.Individual as I
import Penny.Representation
import Penny.Serial
import Penny.Side
import Rainbow.Types
import Rainbow.Colors
import Rainbox (Box, Alignment, Vertical, tableByRows, center, Cell(..), top)

type MakeLines l
  = (Amount -> RepNonNeutralNoSide)
  -> Clatch l
  -> l (Seq (LineTag, Text))

data Column l = Column (Alignment Vertical) (MakeLines l)

newtype Columns l = Columns (Seq (Column l))

instance Monoid (Columns l) where
  mempty = Columns Seq.empty
  mappend (Columns x) (Columns y) = Columns $ x <> y

original
  :: Alignment Vertical
  -> BestField l (Amount -> RepNonNeutralNoSide)
  -> Columns l
original align = Columns . Seq.singleton . Column align . I.original

best
  :: Alignment Vertical
  -> BestField l (Amount -> RepNonNeutralNoSide)
  -> Columns l
best align = Columns . Seq.singleton . Column align . I.best

balance
  :: Alignment Vertical
  -> BestField l (Amount -> RepNonNeutralNoSide)
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
  = CellFormatter Radiant (Amount -> RepNonNeutralNoSide) (LineTag -> TextSpec)

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
    CellFormatter bkgnd converter getTextSpec = getTriple cltch
    mkRow (tag, txt) = Seq.singleton $ Chunk (getTextSpec tag) [txt]
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
noColors :: (Amount -> RepNonNeutralNoSide) -> CellFormatterFromClatch l
noColors converter = CellFormatterFromClatch $ \_ ->
  CellFormatter noColorRadiant converter (const mempty)


-- # High-level formatting

-- | Load data into this record to make a color scheme that has
-- different colors for debits and credits, with an alternating
-- background for odd- and even-numbered postings.
data Colors = Colors
  { debit :: Radiant
  , credit :: Radiant
  , neutral :: Radiant
  , nonLinear :: Radiant
  , notice :: Radiant
  , oddBackground :: Radiant
  , evenBackground :: Radiant
  } deriving (Eq, Ord, Show)

alternating
  :: Colors
  -> (Clatch l -> Amount -> RepNonNeutralNoSide)
  -> CellFormatterFromClatch l
alternating colors converter = CellFormatterFromClatch f
  where
    f clatch = CellFormatter background (converter clatch) formatter
      where
        (Serset (Forward (Serial fwd)) _) = sersetPostFiltered clatch
        background = if even $ naturalToInteger fwd
          then evenBackground colors
          else oddBackground colors
        formatter lineTag = foregroundTextSpecFromRadiant $ case lineTag of
          Linear Nothing -> neutral colors
          Linear (Just Debit) -> debit colors
          Linear (Just Credit) -> credit colors
          NonLinear -> nonLinear colors
          Notice -> notice colors

foregroundTextSpecFromRadiant :: Radiant -> TextSpec
foregroundTextSpecFromRadiant rad = ts
  where
    Chunk ts _ = fore rad

lightBackground :: Colors
lightBackground = Colors
  { debit = blue
  , credit = magenta
  , neutral = black
  , nonLinear = black
  , notice = red
  , oddBackground = noColorRadiant
  , evenBackground = Radiant white8 (Just . Color256 . Just $ 230)
  -- 230: pale yellow
  }

darkBackground :: Colors
darkBackground = Colors
  { debit = blue
  , credit = magenta
  , neutral = white
  , nonLinear = white
  , notice = red
  , oddBackground = noColorRadiant
  , evenBackground = Radiant black8 (Just . Color256 . Just $ 237)
  -- 237: grey
  }
