module Penny.Harvest.Error.Detail where

import Text.Parsec
import qualified Penny.Harvest.Collected.Error.Inline as Inline
import qualified Penny.Harvest.Collected.Error.Final as Final
import qualified Penny.Harvest.Locate.Located as Located
import qualified Penny.Harvest.Transformed.Error as Transformed

data T
  = Parsec ParseError
  -- ^ Could not parse the initial text for a ledger file
  | CollectInline (Located.T Inline.T)
  -- ^ Error while collecting lines
  | CollectFinal Final.T
  -- ^ Error after all lines have been collected
  | Transform Transformed.T
  -- ^ Error during transformation
  deriving Show
