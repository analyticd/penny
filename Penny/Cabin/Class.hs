module Penny.Cabin.Class where

import Data.Text (Text)

import Penny.Lincoln.Boxes (  PostingBox, PriceBox )

class Report a where
  help :: a -> Text
  report ::
    OutputDesc
    -> Colors
    -> [PostingBox t p]
    -> [PriceBox m]
    -> a
    -> Text

data OutputDesc = IsTTY | NotTTY
                deriving Show

-- | The terminal (as described using the TERM environment variable or
-- something similar) supports at least this many colors. Remember,
-- just because the terminal is described this way by TERM does not
-- mean that the program is actually hooked up to such a terminal
-- (output might be going to a file.)
data Colors = Colors0 | Colors8 | Colors256
            deriving Show


