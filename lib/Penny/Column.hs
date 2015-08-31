{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Penny.Column where

import Control.Lens
import Penny.Amount
import Penny.Clatch
import Penny.Commodity
import Penny.Display
import Penny.Popularity
import Penny.Representation
import Penny.Serial
import Penny.Natural
import Penny.Qty
import Penny.Side
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as X
import Rainbox hiding (background)
import Rainbow
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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

data Env = Env
  { _clatch :: Clatch
  , _history :: History
  , _renderer :: Either (Maybe RadCom) (Maybe RadPer)
  , _colors :: Colors
  }

makeLenses ''Env

newtype Column = Column (Env -> Cell)

makeWrapped ''Column

instance Monoid Column where
  mempty = Column (const mempty)
  mappend (Column cx) (Column cy)
    = Column (\a -> (cx a) <> (cy a))

table
  :: History
  -> Either (Maybe RadCom) (Maybe RadPer)
  -> Colors
  -> Seq Column
  -> Seq Clatch
  -> (Seq (Chunk Text))
table hist rend clrs cols cltchs = render . tableByRows $ dataRows
  where
    dataRows = fmap (mkDataRow $) cltchs
    mkDataRow clatch = fmap ($ env) . fmap (^. _Wrapped) $ cols
      where
        env = Env clatch hist rend clrs

background :: Env -> Radiant
background env
  | odd i = env ^. colors.oddBackground
  | otherwise = env ^. colors.evenBackground
  where
    i = env ^. clatch.to postFiltset.forward.to naturalToInteger

spaces :: Int -> Column
spaces i = column (const (X.replicate i . X.singleton $ ' '))

class Colable a where
  column :: (Clatch -> a) -> Column

singleCell
  :: Colable a
  => Env
  -> a
  -> Cell
singleCell env a = ($ env) . (^. _Wrapped) $ column (const a)

instance Colable Text where
  column f = Column cell
    where
      cell env = Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background env) . fore (env ^. colors.nonLinear)
            . chunk . f $ _clatch env
        , _horizontal = top
        , _vertical = left
        , _background = background env
        }

instance Colable Bool where
  column f = Column cell
    where
      cell env = Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background env) . fore fg
            . chunk . X.singleton $ char
        , _horizontal = top
        , _vertical = left
        , _background = background env
        }
        where
          (char, fg)
            | f (_clatch env) = ('T', green)
            | otherwise = ('F', red)

instance Colable Integer where
  column f = column (X.pack . show . f)

instance Colable Unsigned where
  column f = column (naturalToInteger . f)

instance Colable a => Colable (Maybe a) where
  column f = Column g
    where
      g env = case f (env ^. clatch) of
        Nothing -> mempty
        Just v -> singleCell env v

-- | Creates a column for the unsigned portion of the quantity only;
-- does not show the side.  However, the text is correctly color coded
-- for the side.
instance Colable QtyRepAnyRadix where
  column f = Column getCell
    where
      getCell env = Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background env) . fore fgColor
            . chunk . X.pack . ($ "") . display
            . c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
            . f
            $ _clatch env
        , _horizontal = top
        , _vertical = left
        , _background = background env
        }
        where
          fgColor = case maybeSide of
            Nothing -> env ^. colors.neutral
            Just Debit -> env ^. colors.debit
            Just Credit -> env ^. colors.credit
          QtyRepAnyRadix ei = f $ _clatch env
          maybeSide = case ei of
            Left (QtyRep (NilOrBrimPolar coc)) -> case coc of
              Center _ -> Nothing
              OffCenter _ s -> Just s
            Right (QtyRep (NilOrBrimPolar coc)) -> case coc of
              Center _ -> Nothing
              OffCenter _ s -> Just s

instance Colable (Maybe Side) where
  column f = Column getCell
    where
      getCell env = Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background env) . fore fgColor
            . chunk $ txt
        , _horizontal = top
        , _vertical = left
        , _background = background env
        }
        where
          (fgColor, txt) = case f (_clatch env) of
            Nothing -> (env ^. colors.neutral, "--")
            Just Debit -> (env ^. colors.debit, "<")
            Just Credit -> (env ^. colors.credit, ">")


instance Colable Side where
  column f = Column getCell
    where
      getCell env = Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background env) . fore fgColor
            . chunk $ txt
        , _horizontal = top
        , _vertical = left
        , _background = background env
        }
        where
          (fgColor, txt) = case f (_clatch env) of
            Debit -> (env ^. colors.debit, "<")
            Credit -> (env ^. colors.credit, ">")

instance Colable Qty where
  column f = Column cell
    where
      cell env = singleCell env qtyRep
        where
          qtyRep = repQty ei (f (_clatch env))
            where
              ei = either (Left . Just) (Right . Just)
                . selectGrouper
                . Penny.Popularity.groupers (env ^. history)
                $ Nothing

instance Colable Commodity where
  column f = column ((^. _Wrapped) . f)

instance Colable Amount where
  column f = Column cell
    where
      cell env = Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background env) . fore (env ^. foreground)
            . chunk $ txt
        , _horizontal = top
        , _vertical = left
        , _background = background env
        }
        where
          Amount (Commodity cy) qty = f (_clatch env)
