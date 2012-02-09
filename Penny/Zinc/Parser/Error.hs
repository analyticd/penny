module Penny.Zinc.Parser.Error where

import Data.Text (Text)
import qualified System.Console.MultiArg.Error as MAE

data Error = MultiArgError MAE.Expecting MAE.Saw
             | MakeMatcherFactoryError Text
             | DateParseError
             | BadPatternError Text
             | BadNumberError Text
             | BadQtyError Text
             | BadSortKeyError Text
             deriving Show

instance MAE.Error Error where
  parseErr = MultiArgError

