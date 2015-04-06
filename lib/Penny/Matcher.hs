{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.Matcher where

import Control.Applicative
import Data.Bifunctor
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.String
import Rainbow
import Penny.Lincoln.Natural
import Penny.Lincoln.Rep.Digit
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Reader
import Pipes hiding (next)
import qualified Pipes
import qualified Data.Foldable as F

-- | Indicates the current level of nesting.  This is used for logging
-- purposes.  You can increase nesting by using the 'nest'
-- functions.
newtype Nesting = Nesting Unsigned
  deriving (Eq, Ord, Show, Natural)

-- | A nesting level of zero.
nesting0 :: Nesting
nesting0 = Nesting (toUnsigned Zero)

-- | Some text describing a logged action.
newtype Opinion = Opinion [Chunk]
  deriving (Eq, Ord, Show)

instance IsString Opinion where
  fromString = Opinion. (:[]) . fromString

instance Monoid Opinion where
  mempty = Opinion []
  mappend (Opinion x) (Opinion y) = Opinion (x ++ y)

data Verdict = Accepted | Rejected
  deriving (Eq, Ord, Show, Enum, Bounded)

data Level = Proclamation | Notice | Info
  deriving (Eq, Ord, Show, Enum, Bounded)

data Payload
  = Verdict Verdict
  | Missive Level Opinion
  deriving (Eq, Ord, Show)

data Message = Message Nesting Payload
  deriving (Eq, Ord, Show)

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
newtype Matcher s m a
  = Matcher (ReaderT (s, Nesting) (ListT (WriterT (Seq Message) m)) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => Alternative (Matcher s m) where
  empty = Matcher empty
  (Matcher x) <|> (Matcher y) = Matcher (x <|> y)

instance MonadTrans (Matcher s) where
  lift act = Matcher (lift . lift . lift $ act)

instance Monad m => MonadPlus (Matcher s m) where
  mzero = Matcher mzero
  mplus (Matcher x) (Matcher y) = Matcher (x `mplus` y)

-- # Subjects

-- | Gets the current subject under study.
getSubject :: Monad m => Matcher s m s
getSubject = Matcher $ asks fst

-- | Applies a 'Matcher' to a particular subject.
examine
  :: Matcher s m a
  -> s
  -> ListT (WriterT (Seq Message) m) a
examine (Matcher rt) s = runReaderT rt (s, nesting0)

-- | Runs a 'Matcher', but with a subject that is different from that
-- of the current 'Matcher'.  For examples of its use, see 'just' and
-- 'index'.
study
  :: Monad m
  => Matcher s m a
  -> s
  -> Matcher s' m a
study mtcr s = Matcher $ do
  nst <- asks snd
  withReaderT (const (s, nst)) rt
  where
    Matcher rt = do
      r <- mtcr
      return r

-- | Run this Matcher with the logs indented one level.
indent
  :: Monad m
  => Matcher s m a
  -> Matcher s m a
indent (Matcher i) = Matcher $ withReaderT (second next) i

-- # Logging

-- | Logs a single message.
logger :: Monad m => Payload -> Matcher s m ()
logger msg = Matcher $ do
  nstg <- asks snd
  tell . Seq.singleton $ Message nstg msg

-- | Accepts a value; adds an entry to the log.
accept :: Monad m => a -> Matcher s m a
accept a = logger (Verdict Accepted) >> return a

-- | Rejects a value; adds an entry to the log.
reject :: Monad m => Matcher s m a
reject = logger (Verdict Rejected) >> empty

-- | Posts a notice to the log.
notify :: Monad m => Opinion -> Matcher s m ()
notify op = logger (Missive Notice op)

-- | Posts an Info to the log.
inform :: Monad m => Opinion -> Matcher s m ()
inform op = logger (Missive Info op)

-- | Posts a Result indicating that computation is done.
proclaim :: Monad m => Opinion -> Matcher s m ()
proclaim op = logger (Missive Proclamation op)

disjoin
  :: Monad m
  => Matcher s m a
  -- ^
  -> Matcher s m a
  -- ^
  -> Matcher s m a
disjoin x y = do
  inform "Disjunction"
  indent ( inform "disjunction, first branch" >> indent x)
    <|> indent (inform "disjunction, second branch" >> indent y)


conjoin
  :: Monad m
  => Matcher s m a
  -- ^
  -> (a -> Matcher s m b)
  -- ^
  -> Matcher s m b
conjoin ma f = do
  inform "conjunction"
  a <- indent (inform "conjunction, first branch" >> indent ma)
  indent (inform "conjunction, second branch" >> indent (f a))

(&&&) :: Monad m => Matcher s m a -> Matcher s m b -> Matcher s m b
(&&&) x y = conjoin x (const y)
infixr 3 &&&

(|||) :: Monad m => Matcher s m a -> Matcher s m a -> Matcher s m a
(|||) x y = disjoin x y
infixr 2 |||

infixl 1 `disjoin`

--
--
--

-- | Always succeeds and returns a single value of the unit type.
-- Handy with various other combinators:
--
-- @
-- -- Succeeds on any 'Just'
-- 'just' 'always' :: 'Matcher' ('Maybe' a) m ()
--
-- -- Succeeds on any 'Left'
-- 'left' 'always' :: 'Matcher' ('Either' a b) m ()
--
-- @
always :: Monad m => Matcher s m ()
always = proclaim "always matches" >> accept ()

-- | Always fails.
never :: Monad m => Matcher s m a
never = proclaim "always fails" >> reject

-- | Succeeds if the subject is 'Just' and the given matcher succeeds
-- on the 'Just' value.

just :: Monad m => Matcher s m a -> Matcher (Maybe s) m a
just m = do
  mayS <- getSubject
  case mayS of
    Nothing -> proclaim "is Nothing" >> reject
    Just s -> inform "is Just" >> indent (study m s)

-- | Succeeds only if the subject is 'Nothing'.

nothing :: Monad m => Matcher (Maybe a) m ()
nothing = do
  tgt <- getSubject
  case tgt of
    Nothing -> proclaim "is Nothing" >> accept ()
    Just _ -> proclaim "is Just" >> reject

-- | Succeeds if the 'Either' is 'Left' and it matches the given
-- 'Matcher'.

left :: Monad m => Matcher a m r -> Matcher (Either a b) m r
left m = do
  tgt <- getSubject
  case tgt of
    Right _ -> proclaim "is Right" >> reject
    Left l -> inform "is Left" >> indent (study m l)


-- | Succeeds if the 'Either' is 'Left' and it matches the given
-- 'Matcher'.
right :: Monad m => Matcher a m r -> Matcher (Either b a) m r
right m = do
  tgt <- getSubject
  case tgt of
    Left _ -> proclaim "is Left" >> reject
    Right r -> inform "is Right" >> indent (study m r)


-- | Runs the given 'Matcher' on every item in the foldable sequence.
-- Returns all matches.
each
  :: (Monad m, Functor f, Foldable f)
  => Matcher s m a
  -> Matcher (f s) m a
each mtcr = do
  sq <- getSubject
  inform "running matcher for each item in sequence"
  F.asum . fmap (indent . study mtcr) $ sq

-- | Runs the given 'Matcher' on the 'Seq' item at the given index, if
-- the index is in range.  Returns no matches if the index is out of
-- range.
index
  :: Monad m
  => Int
  -> Matcher s m a
  -> Matcher (Seq s) m a
index idx mr = getSubject >>= f
  where
    f sq
      | idx >= 0 && idx < Seq.length sq = do
          inform (fromString $ "applying Matcher so index " ++ show idx)
          indent $ study mr (sq `Seq.index` idx)
      | otherwise = do
          proclaim . fromString $ "index out of range: " ++ show idx
          reject

-- | Filters a 'Seq' using a 'Matcher'.
filterSeq
  :: Monad m
  => Matcher s m a
  -- ^ Filter using this 'Matcher'.  The subject is included in the
  -- result if the 'Matcher' returns a single value of any type.
  -> Seq s
  -- ^ Filter this sequence
  -> m (Seq (Seq Message), Seq s)
  -- ^ Retuns a pair @(ms, ss)@ where
  --
  -- @ms@ is a sequence of sequences, with one sequence of messages
  -- for every original element.  The length of @ms@ is equal to the
  -- length of the input sequence.  @ss@ is the result of the
  -- filtering.
filterSeq mtcr = F.foldlM f (Seq.empty, Seq.empty)
  where
    f (msgs, mtchs) subj = do
      (ei, lg) <- runWriterT . Pipes.next . enumerate . examine mtcr $ subj
      return $ case ei of
        Left _ -> (msgs |> lg, mtchs)
        Right _ -> (msgs |> lg, mtchs |> subj)
