module Penny.Zinc.Error where

import Data.Text (Text)

data Error =
  ParseError Text
  | ReportError Text
