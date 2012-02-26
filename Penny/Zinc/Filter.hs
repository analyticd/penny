module Penny.Zinc.Filter where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous (Exceptional)
import Data.Monoid (mempty, mappend)
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Combinator (option)
import System.Console.MultiArg.Prim (ParserE, throw)

import Penny.Liberty.Error (Error)
import Penny.Liberty.Combinator (runUntilFailure)
import qualified Penny.Liberty.Error as E
import qualified Penny.Liberty.Expressions as X
import qualified Penny.Liberty.Filter as LF
import qualified Penny.Liberty.Operators as Oo
import qualified Penny.Liberty.Seq as PSq
import qualified Penny.Liberty.Sorter as S
import qualified Penny.Liberty.Types as T

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)

data State =
  State { sensitive :: M.CaseSensitive
        , factory :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (T.PostingInfo -> Bool)]
        , postFilter :: [T.PostingInfo] -> [T.PostingInfo]
        , orderer :: S.Orderer }

newState :: State
newState =
  State { sensitive = M.Insensitive
        , factory = \t -> return (M.within M.Insensitive t)
        , tokens = []
        , postFilter = id
        , orderer = mempty }

wrapLiberty ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
wrapLiberty dtz dt rad sp st = let
  toLibSt = LF.State { LF.sensitive = sensitive st
                     , LF.factory = factory st
                     , LF.tokens = tokens st
                     , LF.postFilter = postFilter st }
  fromLibSt libSt = State { sensitive = LF.sensitive libSt
                          , factory = LF.factory libSt
                          , tokens = LF.tokens libSt
                          , postFilter = LF.postFilter libSt
                          , orderer = orderer st }
  in fromLibSt <$> LF.parseOption dtz dt rad sp toLibSt

wrapOrderer :: State -> ParserE Error State
wrapOrderer st = mkSt <$> S.sort where
  mkSt o = st { orderer = o `mappend` (orderer st) }

parseOption ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
parseOption dtz dt rad sp st =
  wrapLiberty dtz dt rad sp st
  <|> wrapOrderer st

parseOptions ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> State
  -> ParserE Error State
parseOptions dtz dt rad sp st =
  option st $ do
    rs <- runUntilFailure (parseOption dtz dt rad sp) st
    if null rs then return st else return (last rs)

data Result =
  Result { resultFactory :: Text -> Exceptional Text (Text -> Bool)
         , resultSensitive :: M.CaseSensitive
         , resultFilter :: [PostingBox] -> [T.PostingInfo] }

parseFilter ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> ParserE Error Result
parseFilter dtz dt rad sep = do
  let st = newState
  st' <- parseOptions dtz dt rad sep st
  p <- case Oo.getPredicate (tokens st') of
    Just pr -> return pr
    Nothing -> throw E.BadExpression
  let f pbs = filter p preFilt where
        preFilt = PSq.postingInfos (orderer st') pbs
  return Result { resultFactory = factory st'
                , resultSensitive = sensitive st'
                , resultFilter = f }
