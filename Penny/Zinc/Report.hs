module Penny.Zinc.Report where

import Control.Applicative ((<|>))
import qualified Control.Monad.Exception.Synchronous as ES
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as X
import System.Console.MultiArg.Prim (ParserE)
import qualified Text.Matchers.Text as T

import Penny.Cabin.Colors (ColorPref)
import qualified Penny.Cabin.Interface as I
import qualified Penny.Liberty.Error as E
import qualified Penny.Shield as S

report ::
  S.Runtime
  -> NE.NonEmpty I.Report
  -> T.CaseSensitive
  -> (X.Text -> ES.Exceptional X.Text (X.Text -> Bool))
  -> ParserE E.Error (I.ReportFunc, ColorPref)
report rt rs c fact = foldl (<|>) first rest where
  toParser r = I.parseReport r rt c fact
  first = toParser (NE.neHead rs)
  rest = map toParser (NE.neTail rs)

