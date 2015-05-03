{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Penny.Column where

import Control.Lens
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as X
import Rainbox
import Rainbow
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Column a = Column
  { _header :: Cell
  , _cell :: a -> Cell
  }

makeLenses ''Column

instance Monoid (Column a) where
  mempty = Column mempty (const mempty)
  mappend (Column hx cx) (Column hy cy)
    = Column (hx <> hy) (\a -> cx a <> cy a)

instance Contravariant Column where
  contramap f (Column h g) = Column h (g . f)

table
  :: Seq (Column a)
  -> Seq a
  -> Seq (Chunk Text)
table cols items
  = render
  . tableByColumns
  . Seq.zipWith (<|) (fmap _header cols)
  . fmap mkColumn
  $ cols
  where
    mkColumn (Column _ mkCell) = fmap (mkCell $) items

spaces :: Int -> Column a
spaces i = mempty & cell .~ const oneRowCell
  where
    oneRowCell = (mempty & rows .~ (Seq.singleton . Seq.singleton
      . chunk . X.replicate i . X.singleton $ ' '))
