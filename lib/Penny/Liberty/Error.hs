module Penny.Liberty.Error where

import Data.Text (Text, pack)

data Error = MakeMatcherFactoryError Text
             | DateParseError
             | BadPatternError Text
             | BadNumberError Text
             | BadQtyError Text
             | BadSortKeyError Text
             | BadComparator Text
             | BadExpression
             | BadColorName Text
             | BadFieldName Text
             | BadBackgroundArg Text
             | UnexpectedWord Text Text
             | BadCommodityError Text
             deriving Show


-- | Barebones for now
display :: Error -> Text
display = pack . show
