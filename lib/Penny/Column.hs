{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGe ScopedTypeVariables #-}
module Penny.Column where

import Control.Lens
import Penny.Clatch
import Penny.Popularity
import Penny.Representation
import Penny.Serial
import Penny.Natural
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
  , _renderer :: Maybe (Either (Maybe RadCom) (Maybe RadPer))
  , _colors :: Colors
  }

makeLenses ''Env


data Column = Column
  { _header :: Colors -> Cell
  , _cell :: Env -> Cell
  }

makeLenses ''Column

instance Monoid Column where
  mempty = Column mempty (const mempty)
  mappend (Column hx cx) (Column hy cy)
    = Column (hx <> hy) (\a -> (cx a) <> (cy a))

table
  :: History
  -> Maybe (Either (Maybe RadCom) (Maybe RadPer))
  -> Colors
  -> Seq Column
  -> Seq Clatch
  -> (Seq (Chunk Text))
table hist rend clrs cols cltchs = render . tableByRows $ rows
  where
    rows = topRow <| dataRows
    topRow = fmap ($ clrs) . fmap _header $ cols
    mkDataRow clatch = fmap ($ env) . fmap _cell $ cols
      where
        env = Env clatch hist rend clrs
    dataRows = fmap (mkDataRow $) cltchs

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

instance Colable Text where
  column f = Column mempty cell
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
  column f = Column mempty cell
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
  column = undefined
