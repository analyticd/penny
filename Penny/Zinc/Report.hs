module Penny.Zinc.Report where

import Control.Applicative ((<|>))
import qualified Control.Monad.Exception.Synchronous as ES
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as X
import System.Console.MultiArg.Prim (ParserE)
import qualified Text.Matchers.Text as T

import Penny.Cabin.Colors (ColorPref)
import qualified Penny.Cabin.Types as CT
import qualified Penny.Liberty.Error as E

report ::
  CT.Runtime
  -> NE.NonEmpty CT.Report
  -> T.CaseSensitive
  -> (X.Text -> ES.Exceptional X.Text (X.Text -> Bool))
  -> ParserE E.Error (CT.ReportFunc, ColorPref)
report rt rs c fact = foldl (<|>) first rest where
  toParser r = CT.parseReport r rt c fact
  first = toParser (NE.neHead rs)
  rest = map toParser (NE.neTail rs)

