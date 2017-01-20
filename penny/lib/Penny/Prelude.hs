module Penny.Prelude
  ( Eq ((==))
  , Either(Left, Right)
  , Maybe(Nothing, Just)
  , FilePath
  , Ord (compare)
  , Show (show)
  , Generic
  , Seq
  , Text
  ) where

import Prelude
  ( Eq((==)), Ord(compare), Show(show),
    Either(Left, Right), Maybe(Nothing, Just))
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Filesystem.Path.CurrentOS (FilePath)
