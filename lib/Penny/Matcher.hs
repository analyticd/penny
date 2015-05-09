{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , study

  -- * Logging
  , accept
  , reject
  , notify
  , inform
  , proclaim
  , indent

  -- * Combining 'Matcher'
  , disjoin
  , conjoin
  , (&&&)
  , (|||)

  -- * For common types
  , always
  , never
  , just
  , nothing
  , left
  , right
  , each
  , index
  , pattern

  -- * Nesting
  , labelNest
  , labelNestMaybe

  -- * Filtering
  , filterSeq

  -- * Rendering messages
  , Chunkable(..)

  -- * Basement
  , logger
  , Nesting(..)
  , Opinion(..)
  , Verdict(..)
  , Level(..)
  , Payload(..)
  , Message(..)
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.String
import Rainbow
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Reader
import Pipes hiding (next, each)
import qualified Pipes
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as X
import Turtle.Pattern

class Chunkable a where
  toChunks :: a -> [Chunk Text] -> [Chunk Text]

-- | Indicates the current level of nesting.  This is used for logging
-- purposes.  You can increase nesting by using the 'nest'
-- functions.
newtype Nesting = Nesting Int
  deriving (Eq, Ord, Show, Enum)

instance Chunkable Nesting where
  toChunks (Nesting i) = ((chunk $ X.replicate i " ") :)


-- | Some text describing a logged action.
newtype Opinion = Opinion [Chunk Text]
  deriving (Eq, Ord, Show)

instance Chunkable Opinion where
  toChunks (Opinion ls) = (ls ++)

instance IsString Opinion where
  fromString = Opinion . (:[]) . chunk . X.pack

instance Monoid Opinion where
  mempty = Opinion []
  mappend (Opinion x) (Opinion y) = Opinion (x ++ y)

data Verdict = Accepted | Rejected
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Chunkable Verdict where
  toChunks v = bracketed $ case v of
    Accepted -> chunk "accepted" & fore green
    Rejected -> chunk "rejected" & fore red
    where
      bracketed ck = ([chunk "[", ck, chunk "]"] ++)

data Level = Proclamation | Notice | Info
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Chunkable Level where
  toChunks lvl = bracketed $ case lvl of
    Proclamation -> "proclaim"
    Notice -> "notice"
    Info -> "info"
    where
      bracketed ck = ([chunk "[", chunk ck, chunk "]"] ++)

data Payload
  = Verdict Verdict
  | Missive Level Opinion
  deriving (Eq, Ord, Show)

instance Chunkable Payload where
  toChunks pay = case pay of
    Verdict v -> toChunks v
    Missive l o -> (toChunks l . (chunk " " :) .  toChunks o)

data Message = Message Nesting Payload
  deriving (Eq, Ord, Show)

instance Chunkable Message where
  toChunks (Message n p) = toChunks n . toChunks p . (chunk "\n":)


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
examine (Matcher rt) s = runReaderT rt (s, Nesting 0)

-- | Extract the first result, if there is one.
observe
  :: Monad m
  => Matcher s m a
  -> s
  -> m (Maybe a)
observe mtcr s = do
  (ei, _) <- runWriterT . Pipes.next . enumerate . examine mtcr $ s
  return $ case ei of
    Left _ -> Nothing
    Right (r, _) -> Just r


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
indent (Matcher i) = Matcher $ withReaderT (second succ) i

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


-- | Creates a 'Matcher' from a Turtle 'Pattern'.
pattern
  :: Monad m
  => Pattern a
  -> Matcher Text m a
pattern pat = do
  txt <- getSubject
  let mtchs = match pat txt
  inform . fromString $ "running text pattern on text: " <> show txt
  (F.asum . fmap (\b -> indent (proclaim "match found" >> accept b)) $ mtchs)
    <|> (proclaim "no matches found" >> reject)

-- | Nests a 'Matcher' within the current 'Matcher', and adds an
-- 'L.Opinion' indicating what is going on.
labelNest
  :: Monad m
  => Opinion
  -- ^ Descriptive text
  -> (t -> m t')
  -- ^ Convert the parent type to the nested type
  -> Matcher t' m a
  -> Matcher t  m a
labelNest op conv mtcr = do
  subj <- getSubject
  t' <- lift . conv $ subj
  inform ("nesting: " <> op)
  indent $ study mtcr t'


labelNestMaybe
  :: Monad m
  => Opinion
  -> (t -> m (Maybe t'))
  -> Matcher t' m a
  -> Matcher t m a
labelNestMaybe op get mtcr = do
  curr <- getSubject
  mayT' <- lift $ get curr
  inform $ "attempting to extract field: " <> op
  case mayT' of
    Nothing -> proclaim "field not found" >> reject
    Just t' -> do
      inform "field found"
      study mtcr t'




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
