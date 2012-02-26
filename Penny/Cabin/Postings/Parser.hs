-- | The Postings command line parser.
module Penny.Cabin.Postings.Parser where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous (Exceptional)
import Data.Monoid (mempty, mappend)
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Prim (ParserE, feed, throw)

import qualified Penny.Liberty.Error as Er
import qualified Penny.Liberty.Expressions as Ex
import qualified Penny.Liberty.Matchers as Ma
import qualified Penny.Liberty.Operands as Od
import qualified Penny.Liberty.Operators as Oo
import qualified Penny.Liberty.PostFilters as Pf
import qualified Penny.Liberty.Seq as Sq
import qualified Penny.Liberty.Sorter as So
import qualified Penny.Liberty.Types as Ty

data State =
  State { sensitive :: M.CaseSensitive
        , factory :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (T.PostingInfo -> Bool)]
        , postFilter :: [T.PostingInfo] -> [T.PostingInfo] }


