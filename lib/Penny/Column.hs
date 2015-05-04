{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGe ScopedTypeVariables #-}
module Penny.Column where

import Control.Lens
import Control.Monad (liftM, liftM2)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as X
import Rainbox
import Rainbow
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T

data Column l a = Column
  { _header :: Cell
  , _cell :: a -> l Cell
  }

makeLenses ''Column

instance Monad l => Monoid (Column l a) where
  mempty = Column mempty (const (return mempty))
  mappend (Column hx cx) (Column hy cy)
    = Column (hx <> hy) (\a -> liftM2 (<>) (cx a) (cy a))

instance Contravariant (Column l) where
  contramap f (Column h g) = Column h (g . f)

table
  :: Monad l
  => Seq (Column l a)
  -> Seq a
  -> l (Seq (Chunk Text))
table cols items
  = liftM (render . tableByColumns . Seq.zipWith (<|) (fmap _header cols))
  $ T.mapM mkColumn cols
  where
    mkColumn (Column _ mkCell) = T.sequence $ fmap (mkCell $) items


spaces :: forall l a. Monad l => Int -> (Column l a)
spaces i = (mempty :: Column l a) & cell .~ const (return oneRowCell)
  where
    oneRowCell = (mempty & rows .~ (Seq.singleton . Seq.singleton
      . chunk . X.replicate i . X.singleton $ ' '))
