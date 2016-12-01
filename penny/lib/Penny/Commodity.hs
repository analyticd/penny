module Penny.Commodity where

import Data.Text (Text)

-- | A commodity, such as @$@, @€@, or a ticker symbol like @F@.
type Commodity = Text
