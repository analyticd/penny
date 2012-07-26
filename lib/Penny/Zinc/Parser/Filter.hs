module Penny.Zinc.Parser.Filter (
  parseFilter
  , Error(LibertyError, TokenParseError)
  , NeedsHelp(NeedsHelp)
  , Result(Result, resultFactory, resultSensitive, sorterFilterer)
  ) where

import Control.Applicative ((<|>), (<$>), Applicative, pure, many)
import Control.Monad ((>=>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Monoid (mempty, mappend)
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Copper as Cop
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as X

import qualified Penny.Zinc.Parser.Defaults as D
import qualified Penny.Zinc.Parser.Defaults as Defaults

-- | Parses all filtering options. Returns a parser that contains an
-- Exception if some error occurred after parsing the options, or a
-- Success with a result if the parse was successful.
parseFilter ::
  Defaults.T
  -> Parser (Ex.Exceptional Error (Either NeedsHelp Result))
parseFilter d = fmap f (many parser) where
  f ls =
    let k = foldl (>=>) return ls
    in case k (newState d) of
      Ex.Success st' ->
        if help st'
        then return . Left $ NeedsHelp
        else
          case Ly.parsePredicate . tokens $ st' of
            Nothing -> Ex.throw TokenParseError
            Just pdct ->
              let fn = Ly.xactionsToFiltered pdct
                       (postFilter st') (orderer st')
                  r = Result { resultFactory = factory st'
                             , resultSensitive = sensitive st'
                             , sorterFilterer = fn }
              in return . Right $ r
      Ex.Exception e -> Ex.Exception e

data Error = LibertyError Ly.Error
             | TokenParseError
             deriving Show

-- | Returned if the user requested help.
data NeedsHelp = NeedsHelp
                 deriving Show

-- | Indicates the result of a successful parse of filtering options.
data Result =
  Result { resultFactory :: M.CaseSensitive
                            -> Text -> Ex.Exceptional Text (Text -> Bool)
           -- ^ The factory indicated, so that it can be used in
           -- subsequent parses of the same command line.

         , resultSensitive :: M.CaseSensitive
           -- ^ Indicated case sensitivity, so that it can be used in
           -- subsequent parses of the command line.
           
         , sorterFilterer :: [L.Transaction] -> [L.Box Ly.LibertyMeta]
           -- ^ Applied to a list of Transaction, will sort and filter
           -- the transactions and assign them LibertyMeta.
         }


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
  Defaults.T
  -> State
newState d =
  State { sensitive = D.sensitive d
        , factory = D.factory d
        , tokens = []
        , postFilter = []
        , orderer = mempty
        , help = False
        , currentTime = D.currentTime d
        , defaultTimeZone = D.defaultTimeZone d
        , radGroup = D.radGroup d }

parser :: Parser (State -> Ex.Exceptional Error State)
parser =
  operand
  <|> parsePostFilter
  <|> impurify parseMatcherSelect
  <|> impurify parseCaseSelect
  <|> impurify parseOperator
  <|> parseSort
  <|> impurify parseHelp

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
            Ex.Success pf ->
              let ls' = postFilter st ++ [pf]
              in return st { postFilter = ls' }
      in g

impurify ::
  (Functor f, Applicative a)
  => f (b -> b)
  -> f (b -> a b)
impurify = fmap (pure .)

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

parseOperator :: Parser (State -> State)
parseOperator = f <$> Ly.parseOperator
  where
    f tok = g
      where
        g st = st { tokens = tokens st ++ [tok] }

parseSort :: Parser (State -> Ex.Exceptional Error State)
parseSort = f <$> Ly.parseSort
  where
    f exOrd = g
      where
        g st = case exOrd of
          Ex.Exception e -> Ex.throw . LibertyError $ e
          Ex.Success o ->
            return st { orderer = mappend o (orderer st) }

parseHelp :: Parser (State -> State)
parseHelp = option ["help"] ['h'] (C.NoArg f)
  where
    f st = st { help = True }

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
