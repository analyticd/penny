module Penny.Dump where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X
import qualified Text.Show.Pretty as Pretty
import Rainbow (Chunk)
import qualified Rainbow

import Penny.Clatch.Types
import Penny.Clatcher (Report)
import Penny.Cursor
import Penny.Pretty

printReport
  :: Seq (Clatch (Maybe Cursor))
  -> Chunk Text
printReport
  = Rainbow.chunk
  . flip X.snoc '\n'
  . X.pack
  . Pretty.valToStr
  . mkVal
  where
    mkVal = prettySeq (prettyClatch Pretty.prettyVal)

dump :: Report
dump _ _ _ clatches = Seq.singleton (printReport clatches)
