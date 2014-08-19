module Penny.Copper.Flag where

import qualified Data.Text as X
import Penny.Copper.Render
import qualified Penny.Copper.Posting as P

-- | A Flag, with no newlines and no closing square brace characters.
newtype Flag = Flag { unFlag :: P.Flag }
  deriving (Eq, Ord, Show)

toCopperFlag :: P.Flag -> 
