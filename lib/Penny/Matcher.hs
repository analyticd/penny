{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Matchers.
--
-- Here are the core components of 'Matcher' and matchers for prelude
-- types.  The idea is that this module could eventually be extracted
-- from Penny and be a package that Penny depends on.
module Penny.Matcher
  ( -- * Matcher
    Matcher(..)

    -- * Subjects
  , getSubject
  , examine
  , observe
  , observeAll
  , study
  , tunnel

  -- * Combining 'Matcher'
  , invert
  , feed

  -- * For common types
  , true
  , false
  , just
  , nothing
  , left
  , right
  , each
  , index
  , pattern

  -- * Filtering
  , filter
  ) where

import Control.Applicative
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad.Writer
import Control.Monad.Reader
import Pipes hiding (next, each)
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified Data.Foldable as F
import Data.Text (Text)
import Turtle.Pattern
import Control.Lens hiding (Level, each, index)
import Prelude hiding (filter)

-- | A 'Matcher' is a computation that, when run with a value known as
-- the @subject@, returns any number of @matches@.  The type variables
-- represent:
--
-- @s@ - the subject
--
-- @m@ - an arbitrary underlying monad
--
-- @a@ - the type of the match returned.
--
-- To indicate that a 'Matcher' returns no matches, use 'empty'.  To
-- successfully return a match, use 'pure' or 'return'.
--
-- 'Matcher' is an instance of 'Applicative' and 'Monad', so you can
-- combine computations using those typeclass methods.  It is also an
-- instance of 'Alternative', which provides disjunction.  That is,
-- @matchA '<|>' matchB@ creates a new 'Matcher' that will return all
-- matches generated both by @matchA@ and @matchB@.
--
-- For conjunction--that is, to indicate that a series of matches must
-- all succeed in order for a larger match to succeed--use '>>=',
-- '>>', and their friend, @do@ notation.  For example, @matchA >>
-- matchB@ means that both @matchA@ and @matchB@ must match;
-- otherwise, no matches are returned.
--
-- The 'Matcher' also has logging capabilities, which is handy for
-- displaying progress reports to the user.
--
-- 'Matcher' is a monad transformer.
--
-- You should not have to worry about the innards of 'Matcher';
-- bindings are provided in this module that should allow you to
-- interact with computations of this type.
newtype Matcher s m a = Matcher (ReaderT s (ListT m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => Alternative (Matcher s m) where
  empty = Matcher Control.Applicative.empty
  (Matcher x) <|> (Matcher y) = Matcher (x <|> y)

instance MonadTrans (Matcher s) where
  lift act = Matcher (lift . lift $ act)

instance Monad m => MonadPlus (Matcher s m) where
  mzero = Matcher mzero
  mplus (Matcher x) (Matcher y) = Matcher (x `mplus` y)

-- # Subjects

-- | Gets the current subject under study.
getSubject :: Monad m => Matcher s m s
getSubject = Matcher ask

-- | Applies a 'Matcher' to a particular subject.
examine
  :: Matcher s m a
  -> s
  -> ListT m a
examine (Matcher rt) s = runReaderT rt s

-- | Extract the first result, if there is one.
observe
  :: Monad m
  => Matcher s m a
  -> s
  -> m (Maybe a)
observe mtcr s = do
  ei <- Pipes.next . enumerate . examine mtcr $ s
  return $ case ei of
    Left _ -> Nothing
    Right (r, _) -> Just r

-- | Extract all results.
observeAll
  :: Monad m
  => Matcher s m a
  -> s
  -> m (Seq a)
observeAll mtcr = liftM Seq.fromList
  . Pipes.toListM . enumerate
  . examine mtcr


-- | Runs a 'Matcher', but with a subject that is different from that
-- of the current 'Matcher'.  For examples of its use, see 'just' and
-- 'index'.
study
  :: Monad m
  => Matcher s m a
  -> s
  -> Matcher s' m a
study mr s = tunnel (const (return s)) `feed` mr

-- | Lifts a function into a 'Matcher'.  All subjects are morphed by
-- the function and the result is passed through as is.
tunnel :: Monad m => (a -> m b) -> Matcher a m b
tunnel f = getSubject >>= lift . f

-- | Reverses the result of the given matcher.
invert :: Monad m => Matcher s m a -> Matcher s m ()
invert k = do
  mayR <- optional k
  case mayR of
    Nothing -> return ()
    Just _ -> mzero

-- | Connects two 'Matcher' together.  Forms a category, with 'feed'
-- being the composition operator and 'getSubject' being the identity.

feed :: Monad m => Matcher a m b -> Matcher b m c -> Matcher a m c
feed mab mbc = Matcher . ReaderT $ \subj -> run mab subj >>= run mbc
  where
    run (Matcher mr) s = runReaderT mr s

infixr 1 `feed`

--
--
--

-- | Succeeds if the subject is 'True'.
true :: Monad m => Matcher Bool m ()
true = getSubject >>= f where
  f True = return ()
  f False = mzero

-- | Succeeds if the subject is 'False'.
false :: Monad m => Matcher Bool m ()
false = getSubject >>= f where
  f True = mzero
  f False = return ()

-- | Extracts the value of the 'Just'; fails if the 'Maybe' is 'Nothing'.
just :: Monad m => Matcher (Maybe s) m s
just = getSubject >>= maybe mzero return

-- | Succeeds only if the subject is 'Nothing'.

nothing :: Monad m => Matcher (Maybe a) m ()
nothing = getSubject >>= maybe (return ()) (const mzero)

-- | Extracts the 'Left' value if there is one.

left :: Monad m => Matcher (Either a b) m a
left = getSubject >>= either return (const mzero)

-- | Extracts the 'Right' value if there is one.
right :: Monad m => Matcher (Either a b) m b
right = getSubject >>= either (const mzero) return


-- | Runs the given 'Matcher' on every item in the foldable sequence.
-- Returns all matches.
each
  :: (Monad m, Functor f, Foldable f)
  => Matcher s m a
  -> Matcher (f s) m a
each mtcr = do
  sq <- getSubject
  F.asum . fmap (study mtcr) $ sq

index
  :: Monad m
  => Int
  -> Matcher (Seq s) m s
index idx = getSubject >>= f
  where
    f sq
      | idx >= 0 && idx < Seq.length sq = return (sq `Seq.index` idx)
      | otherwise = mzero


-- | Creates a 'Matcher' from a Turtle 'Pattern'.
pattern
  :: Monad m
  => Pattern a
  -> Matcher Text m a
pattern pat = getSubject >>= F.asum . fmap return . match pat

-- | Filters a 'Seq' using a 'Matcher'.
filter
  :: Monad m
  => Matcher s m a
  -- ^ Filter using this 'Matcher'.  The subject is included in the
  -- result if the 'Matcher' returns a single value of any type.
  -> Seq s
  -- ^ Filter this sequence
  -> m (Seq s)
filter mtcr = F.foldlM f Seq.empty
  where
    f mtchs subj = do
      ei <- Pipes.next . enumerate . examine mtcr $ subj
      return $ case ei of
        Left _ -> mtchs
        Right _ -> mtchs |> subj
