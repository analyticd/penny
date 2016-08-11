{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | A bare transaction; that is, one without any serial numbers.
--
-- Contrast a 'Penny.Core.Transaction', which has serial numbers.
module Penny.Transaction where

import Penny.Ents
import Penny.Tranche

import qualified Control.Lens as Lens

-- | A bare transaction; that is, one without any serial numbers.  In
-- contrast, 'Penny.Core.Transaction' includes serial numbers for
-- both the top line and the postings.
data Transaction a = Transaction
  { _topLine :: TopLine a
  , _postings :: Balanced (Postline a)
  } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''Transaction

