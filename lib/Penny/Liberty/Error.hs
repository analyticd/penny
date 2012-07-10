module Penny.Liberty.Error where

import Data.Text (Text)


-- | Barebones for now
display :: Error -> Text
display = pack . show
