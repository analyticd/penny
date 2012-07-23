module Penny.Zinc.Parser.Filter where

import Control.Applicative ((<|>), (<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Monoid (mempty, mappend)
import Data.Text (Text, pack)
import qualified Text.Matchers.Text as M
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Copper as Cop
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as X

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (RadGroup)



data Error = LibertyError Ly.Error
             deriving Show

data State =
  State { sensitive :: M.CaseSensitive
        , factory :: M.CaseSensitive
                     -> Text -> Ex.Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (L.PostFam -> Bool)]
        , postFilter :: [Ly.PostFilterFn]
        , orderer :: Ly.Orderer
        , help :: Bool
        , currentTime :: L.DateTime
        , defaultTimeZone :: Cop.DefaultTimeZone
        , radGroup :: Cop.RadGroup }

newState ::
  L.DateTime
  -> Cop.DefaultTimeZone
  -> Cop.RadGroup
  -> State
newState time dtz rg =
  State { sensitive = M.Insensitive
        , factory = \c t -> return (M.within c t)
        , tokens = []
        , postFilter = []
        , orderer = mempty
        , help = False
        , currentTime = time
        , defaultTimeZone = dtz
        , radGroup = rg }

option :: [String] -> [Char] -> C.ArgSpec a -> Parser a
option ss cs a = C.parseOption [C.OptSpec ss cs a]

operand :: Parser (State -> Ex.Exceptional Error State)
operand = f <$> Ly.parseOperand
  where
    f lyFn =
      let g st =
            let r = lyFn (currentTime st) (defaultTimeZone st)
                    (radGroup st) (sensitive st) (factory st)
            in case r of
              Ex.Exception e -> Ex.throw . LibertyError $ e
              Ex.Success (X.Operand o) ->
                let tok' = tokens st ++ [X.TokOperand o]
                in return st { tokens = tok' }
      in g
                   
parsePostFilter :: Parser (State -> Ex.Exceptional Error State)
parsePostFilter = f <$> Ly.parsePostFilter
  where
    f lyResult =
      let g st = case lyResult of
            Ex.Exception e -> Ex.throw . LibertyError $ e
            Ex.Success g ->
              let ls' = postFilter st ++ [g]
              in return st { postFilter = ls' }
      in g

impurify ::
  (Functor f, 

parseMatcherSelect :: Parser (State -> State)
parseMatcherSelect = f <$> Ly.parseMatcherSelect
  where
    f fty = g
      where
        g st = st { factory = fty }

parseCaseSelect :: Parser (State -> State)
parseCaseSelect = f <$> Ly.parseCaseSelect
  where
    f sel = g
      where
        g st = st { sensitive = sel }

parseOperator :: Parser (State -> Ex.Exceptional a State)
parseOperator = undefined

{-
wrapLiberty ::
  DefaultTimeZone
  -> DateTime
  -> RadGroup
  -> State
  -> ParserE Error State
wrapLiberty dtz dt rg st = let
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
  in fromLibSt <$> LF.parseOption dtz dt rg toLibSt

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
  -> RadGroup
  -> State
  -> ParserE Error State
parseOption dtz dt rg st =
  wrapLiberty dtz dt rg st
  <|> wrapOrderer st
  <|> wrapHelp st

parseOptions ::
  DefaultTimeZone
  -> DateTime
  -> RadGroup
  -> State
  -> ParserE Error State
parseOptions dtz dt rg st =
  option st $ do
    rs <- runUntilFailure (parseOption dtz dt rg) st
    if null rs then return st else return (last rs)

data Result =
  Result { resultFactory :: M.CaseSensitive
                            -> Text -> Exceptional Text (Text -> Bool)
         , resultSensitive :: M.CaseSensitive
         , sorterFilterer :: [PostingBox] -> [T.PostingInfo] }

parseFilter ::
  DefaultTimeZone
  -> DateTime
  -> RadGroup
  -> ParserE Error (Either NeedsHelp Result)
parseFilter dtz dt rg = do
  st' <- parseOptions dtz dt rg newState
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
-}
