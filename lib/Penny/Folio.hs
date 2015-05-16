{-# LANGUAGE TemplateHaskell #-}

module Penny.Folio
  ( -- * Folio type
    Folio(..)

    -- * Folio Lenses
  , pre
  , post
  , report
  ) where

import Control.Lens.TH
import Data.Sequence (Seq)
import Rainbow
import Data.Text (Text)

-- | Holds all the data used in a final report in the clatcher.
data Folio = Folio
  { _pre :: Seq (Chunk Text)
  -- ^ The pre-filter report.
  , _post :: Seq (Chunk Text)
  -- ^ The post-filter report.
  , _report :: Seq (Chunk Text)
  -- ^ The main report.
  } deriving (Eq, Ord, Show)

makeLenses ''Folio
