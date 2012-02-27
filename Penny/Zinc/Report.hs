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
  NE.NonEmpty CT.Report
  -> T.CaseSensitive
  -> (X.Text -> ES.Exceptional X.Text (X.Text -> Bool))
  -> ParserE E.Error (CT.ReportFunc, ColorPref)
report rs c fact = foldl (<|>) first rest where
  toParser r = CT.parseReport r c fact
  first = toParser (NE.neHead rs)
  rest = map toParser (NE.neTail rs)

