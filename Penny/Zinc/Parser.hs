module Penny.Zinc.Parser where

import Control.Monad.Exception.Synchronous (
  Exceptional(Exception, Success))
import Data.Monoid (mempty, mappend)
import Data.Monoid.Extra (Orderer(appOrderer))
import Data.Text (Text)
import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Prim (ParserE)
import qualified Text.Matchers.Text as M

import Penny.Copper.Meta ( TransactionMeta, PostingMeta )
import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Zinc.Expressions as X

data Error = MultiArgError E.SimpleError
             | MakeMatcherFactoryError Text
             deriving Show

instance E.Error Error where
  parseErr exp saw = MultiArgError (E.SimpleError exp saw)

type PostBox = PostingBox TransactionMeta PostingMeta

data State =
  State { sensitive :: M.CaseSensitive
        , matcher :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: X.Expression (PostBox -> Bool)
        , sorter :: Orderer (PostBox -> Ordering) }

blankState :: State
blankState = State { sensitive = M.Insensitive
                   , matcher = return . M.within M.Insensitive
                   , tokens = mempty
                   , sorter = mempty }
