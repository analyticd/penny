module Penny.Zinc.Parser where

import Data.Text (Text)
import qualified System.Console.MultiArg.Error as E
import qualified Text.Matchers.Text as M

import Penny.Copper.Meta ( TransactionMeta, PostingMeta )
import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Zinc.Expressions as X

data Error = MultiArgError E.SimpleError
             | MakeMatcherFactoryError Text
             deriving Show

type PostBox = PostingBox TransactionMeta PostingMeta

data State =
  State { sensitive :: M.CaseSensitive
        , matcher :: Text -> Bool
        , tokens :: X.Expression (PostBox -> Bool)
        , sorter :: PostBox -> Ordering }
