module Penny.Dump where

import Data.Text (Text)
import qualified Data.Text as X
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty
import Rainbow (Chunk)
import qualified Rainbow

printReport
  :: PrettyVal a
  => a
  -> Chunk Text
printReport = Rainbow.chunk . flip X.snoc '\n' . X.pack . Pretty.dumpStr
