-- | Some combinators that can be useful for working with parsers.
module Penny.Liberty.Combinator where

import qualified Data.Text as X
import qualified Penny.Liberty.Error as LE
import System.Console.MultiArg.Error as E
import System.Console.MultiArg.Prim as P
import System.Console.MultiArg.Combinator (option)

-- | Fails without consuming any input if the parser
-- succeeds. Succeeds if the parser fails without consuming any
-- input. Fails if the parser fails while consuming input.
failOnSuccess ::
  (Monad m, E.Error e)
  => (a -> P.ParserT s e m a)
  -> a
  -> P.ParserT s e m ()
failOnSuccess pf i = do
  r <- option False ((lookAhead (pf i)) >> return True)
  if r then P.genericThrow else return ()


-- | Repetitively runs a parser until it fails. Succeeds if the last
-- run of the parser fails without consuming any input. Fails if the
-- last run of the parser fails and consumes input.
runUntilFailure ::
  (Monad m, E.Error e)
  => (a -> P.ParserT s e m a)
  -> a
  -> P.ParserT s e m [a]
runUntilFailure f = P.feed f (failOnSuccess f)

-- | Succeeds and consumes the next word if the next word on the
-- command line is the word given. If the next word is something else,
-- or if there is no next word, fails without consuming any input.
nextWordIs ::
  (Monad m)
  => X.Text
  -> P.ParserT s LE.Error m X.Text
nextWordIs t = do
  w <- P.lookAhead P.nextArg
  if w == t
    then nextArg
    else P.throw $ LE.UnexpectedWord t w
