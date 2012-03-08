module Penny.Liberty.Matchers where

import Control.Applicative ((<$>), (<*>), pure, (<|>), (<$))
import Control.Monad.Exception.Synchronous (Exceptional)
import Data.Text (Text, pack)
import System.Console.MultiArg.Combinator
  (mixedNoArg, longNoArg)
import System.Console.MultiArg.Option (makeLongOpt, makeShortOpt)
import System.Console.MultiArg.Prim (ParserE)
import qualified Text.Matchers.Text as M

import Penny.Liberty.Error (Error)

type MatcherFactory = M.CaseSensitive
                      -> Text -> Exceptional Text (Text -> Bool)

parser ::
  M.CaseSensitive
  -> MatcherFactory
  -> ParserE Error (M.CaseSensitive, MatcherFactory)
parser c m =
  ((,) <$> caseInsensitive <*> pure m)
  <|> ((,) <$> caseSensitive <*> pure m)
  <|> ((,) <$> pure c <*> within)
  <|> ((,) <$> pure c <*> pcre)
  <|> ((,) <$> pure c <*> posix)
  <|> ((,) <$> pure c <*> exact)

caseInsensitive :: ParserE Error M.CaseSensitive
caseInsensitive = M.Insensitive <$ p where
  p = mixedNoArg lo [] [so]
  lo = makeLongOpt . pack $ "case-insensitive"
  so = makeShortOpt 'i'

caseSensitive :: ParserE Error M.CaseSensitive
caseSensitive = M.Sensitive <$ p where
  p = mixedNoArg lo [] [so]
  lo = makeLongOpt . pack $ "case-sensitive"
  so = makeShortOpt 'I'

within :: ParserE Error MatcherFactory
within = f <$ p where
  p = longNoArg (makeLongOpt . pack $ "within")
  f c t = return (M.within c t)

pcre :: ParserE Error MatcherFactory
pcre = f <$ p where
  p = longNoArg (makeLongOpt . pack $ "pcre")
  f c = M.pcre c

posix :: ParserE Error MatcherFactory
posix = f <$ p where
  p = longNoArg (makeLongOpt . pack $ "posix")
  f c = M.tdfa c

exact :: ParserE Error MatcherFactory
exact = f <$ (longNoArg (makeLongOpt . pack $ "exact")) where
  f c t = return (M.exact c t)




