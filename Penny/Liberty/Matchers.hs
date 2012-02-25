module Penny.Liberty.Matchers where

import Control.Applicative ((<$>), (<*>), pure, (<|>))
import Control.Monad.Exception.Synchronous (
  Exceptional(Exception, Success))
import Data.Text (Text, pack, unpack)
import System.Console.MultiArg.Combinator
  (mixedNoArg, mixedOneArg, longOneArg, longNoArg, longTwoArg,
   mixedTwoArg)
import System.Console.MultiArg.Option (makeLongOpt, makeShortOpt)
import System.Console.MultiArg.Prim (ParserE, throw)
import qualified Text.Matchers.Text as M

import Penny.Liberty.Error (Error)

type MatcherFactory = Text -> Exceptional Text (Text -> Bool)

parser ::
  M.CaseSensitive
  -> MatcherFactory
  -> ParserE Error (M.CaseSensitive, MatcherFactory)
parser c m =
  ((,) <$> caseInsensitive <*> pure m)
  <|> ((,) <$> caseSensitive <*> pure m)
  <|> ((,) <$> pure c <*> within c)
  <|> ((,) <$> pure c <*> pcre c)
  <|> ((,) <$> pure c <*> posix c)
  <|> ((,) <$> pure c <*> exact c)

caseInsensitive :: ParserE Error M.CaseSensitive
caseInsensitive = do
  let lo = makeLongOpt . pack $ "case-insensitive"
      so = makeShortOpt 'i'
  _ <- mixedNoArg lo [] [so]
  return M.Insensitive

caseSensitive :: ParserE Error M.CaseSensitive
caseSensitive = do
  let lo = makeLongOpt . pack $ "case-sensitive"
      so = makeShortOpt 'I'
  _ <- mixedNoArg lo [] [so]
  return M.Sensitive

within :: M.CaseSensitive -> ParserE Error MatcherFactory
within s = do
  let lo = makeLongOpt . pack $ "within"
  _ <- longNoArg lo
  return $ \t -> return (M.within s t)

pcre :: M.CaseSensitive -> ParserE Error MatcherFactory
pcre s = do
  let lo = makeLongOpt . pack $ "pcre"
  _ <- longNoArg lo
  return $ \t -> (M.pcre s) t

posix :: M.CaseSensitive -> ParserE Error MatcherFactory
posix s = do
  let lo = makeLongOpt . pack $ "posix"
  _ <- longNoArg lo
  return $ \t -> (M.tdfa s) t

exact :: M.CaseSensitive -> ParserE Error MatcherFactory
exact s = do
  let lo = makeLongOpt . pack $ "exact"
  _ <- longNoArg lo
  return $ \t -> return (M.exact s t)



