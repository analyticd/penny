module Penny.Zinc.Parser.Filter where

import Control.Applicative ((<|>), (<$>))
import Control.Monad.Exception.Synchronous (Exceptional)
import Data.Monoid (mempty, mappend)
import Data.Text (Text, pack)
import qualified Text.Matchers.Text as M
import System.Console.MultiArg.Combinator (option, mixedNoArg)
import System.Console.MultiArg.Option (makeLongOpt, makeShortOpt)
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

data NeedsHelp = NeedsHelp

data Help = Help | NoHelp

data State =
  State { sensitive :: M.CaseSensitive
        , factory :: Text -> Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (T.PostingInfo -> Bool)]
        , postFilter :: [T.PostingInfo] -> [T.PostingInfo]
        , orderer :: S.Orderer
        , help :: Help }

newState :: State
newState =
  State { sensitive = M.Insensitive
        , factory = \t -> return (M.within M.Insensitive t)
        , tokens = []
        , postFilter = id
        , orderer = mempty 
        , help = NoHelp }

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
                          , orderer = orderer st
                          , help = help st }
  in fromLibSt <$> LF.parseOption dtz dt rad sp toLibSt

wrapOrderer :: State -> ParserE Error State
wrapOrderer st = mkSt <$> S.sort where
  mkSt o = st { orderer = o `mappend` (orderer st) }

helpOpt :: ParserE Error ()
helpOpt = do
  let lo = makeLongOpt . pack $ "help"
      so = makeShortOpt 'h'
  _ <- mixedNoArg lo [] [so]
  return ()

wrapHelp :: State -> ParserE Error State
wrapHelp st = (\_ -> st { help = Help }) <$> helpOpt

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
  <|> wrapHelp st

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
         , sorterFilterer :: [PostingBox] -> [T.PostingInfo] }

parseFilter ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> ParserE Error (Either NeedsHelp Result)
parseFilter dtz dt rad sep = do
  st' <- parseOptions dtz dt rad sep newState
  case help st' of
    Help -> return . Left $ NeedsHelp
    NoHelp -> do
      p <- case Oo.getPredicate (tokens st') of
        Just pr -> return pr
        Nothing -> throw E.BadExpression
      let f = sortFilterAndPostFilter (orderer st') p (postFilter st')
          r = Result { resultFactory = factory st'
                     , resultSensitive = sensitive st'
                     , sorterFilterer = f }
      return . Right $ r

sortFilterAndPostFilter ::
  S.Orderer
  -> (T.PostingInfo -> Bool)
  -> ([T.PostingInfo] -> [T.PostingInfo])
  -> [PostingBox] -> [T.PostingInfo]
sortFilterAndPostFilter o p pf =
  pf
  . filter p
  . PSq.sortedPostingInfos o