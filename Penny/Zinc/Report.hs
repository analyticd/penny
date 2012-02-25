module Penny.Zinc.Report where

import Control.Applicative ((<|>))
import qualified Control.Monad.Exception.Synchronous as ES
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as X
import qualified Penny.Cabin.Class as C
import Penny.Cabin.Colors (Colors)
import qualified Penny.Liberty.Error as E
import qualified Text.Matchers.Text as T
import System.Console.MultiArg.Prim (ParserE)

report ::
  NE.NonEmpty C.Report
  -> T.CaseSensitive
  -> (X.Text -> ES.Exceptional X.Text (X.Text -> Bool))
  -> ParserE E.Error (C.ReportFunc, Colors)
report rs c fact = foldl (<|>) first rest where
  toParser r = C.printReport r c fact
  first = toParser (NE.neHead rs)
  rest = map toParser (NE.neTail rs)

