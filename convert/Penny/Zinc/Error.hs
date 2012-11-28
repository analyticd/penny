module Penny.Zinc.Error where

import Data.Text (Text, pack)

data Error =
  ParseError Text
  | ReportError Text
  deriving Show

printError :: Error -> Text
printError = pack . show
