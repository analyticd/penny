-- | Some combinators that can be useful for working with parsers.
module Penny.Liberty.Combinator where

import Control.Applicative ((<|>))
import System.Console.MultiArg.Error as E
import System.Console.MultiArg.Prim as P

-- | Fails without consuming any input if the parser
-- succeeds. Succeeds if the parser fails without consuming any
-- input. Fails if the parser fails while consuming input.
failOnSuccess ::
  (Monad m, E.Error e)
  => (a -> P.ParserT s e m a)
  -> a
  -> P.ParserT s e m ()
failOnSuccess pf i = let p = pf i in
  (P.lookAhead p >> P.genericThrow) <|> return ()


-- | Repetitively runs a parser until it fails. Succeeds if the last
-- run of the parser fails without consuming any input. Fails if the
-- last run of the parser fails and consumes input.
runUntilFailure ::
  (Monad m, E.Error e)
  => (a -> P.ParserT s e m a)
  -> a
  -> P.ParserT s e m [a]
runUntilFailure f = P.feed f (failOnSuccess f)

