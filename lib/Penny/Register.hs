{-# LANGUAGE TemplateHaskell #-}

module Penny.Register where

import Control.Lens
  ( makeLenses
  )
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import Penny.Column
  ( Column
  , Colors
  , table
  )
import Penny.Report
  ( Report (printReport)
  )

data Register = Register
  { _colors :: Colors
  , _columns :: Seq Column
  }

makeLenses ''Register

instance Monoid Register where
  mempty = Register mempty mempty
  mappend (Register x1 y1) (Register x2 y2) =
    Register (x1 <> x2) (y1 <> y2)

instance Report Register where
  printReport (Register colors columns) history clatches
    = table history colors columns clatches
