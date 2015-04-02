{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Penny.Lincoln.Matcher
  ( -- * Matcher type
    Matcher(..)

    -- * Applying matchers
  , applyMatcher
  , atLeastOne

  -- * Predicates
  , predicate
  , predicateL

    -- * Subjects
  , getSubject
  , localSubject

  -- * Accepting values

  -- | When you have found a successful match, use these functions.
  -- You can also simply use 'pure' or 'return'; the functions below
  -- have the advantage of adding entries to the log.
  , accept
  , acceptL

  -- * Rejecting values

  -- | When you have not found a match, use these functions.  You can
  -- also simply use 'mzero' or 'empty'; the functions below have the
  -- advantage of also adding entries to the log.
  , reject
  , rejectL

  -- * Adding to the log
  , notice
  , inform

  -- * Nesting
  , nest
  , nestL
  , labeled

  -- * Turtle Patterns
  , pattern

  -- * Constants
  , always
  , never

  -- * Matching on common types
  , just
  , nothing
  , each
  , index

  -- * Basement

  -- | You shouldn't need the things down here, but if needed here
  -- they are.

  , Nesting(..)
  , nesting0
  , Opinion(..)
  , Verdict(..)
  , Message(..)
  , currentNesting
  , logger
  , acceptW
  , rejectW
  , nestW
  , predicateW
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Text as X
import qualified Data.Foldable as F
import Pipes (ListT, enumerate)
import qualified Pipes
import Turtle.Pattern
import Penny.Lincoln.Natural
import Penny.Lincoln.Rep.Digit
import Rainbow
import Control.Monad.Reader
import Control.Monad.Writer
import Data.String
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq

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
-- @matchA <|> matchB@ creates a new 'Matcher' that will return all
-- matches generated both by @matchA@ and @matchB@.
--
-- For conjunction--that is, to indicate that a series of matches must
-- all succeed in order for a larger matcho to succeed--use '>>=',
-- '>>', and their friend, @do@ notation.  For example, @matchA >>
-- matchB@ means that both @matchA@ and @matchB@ must match;
-- otherwise, no matches are returned.
--
-- The 'Matcher' also has logging capabilities, which is handy for
-- displaying progress reports to the user.
--
-- You should not have to worry about the innards of 'Matcher';
-- bindings are provided in this module that should allow you to
-- interact with computations of this type.
newtype Matcher t m a
  = Matcher (WriterT (Seq Message) (ReaderT (t, Nesting) (ListT m)) a)
  deriving (Functor, Applicative, Monad)

instance Monad m => Alternative (Matcher t m) where
  empty = Matcher . WriterT . ReaderT $ \_ -> mzero
  (Matcher (WriterT (ReaderT fx))) <|> (Matcher (WriterT (ReaderT fy)))
    = Matcher . WriterT . ReaderT $ \env ->
    (fx env) <|> (fy env)

instance MonadIO m => MonadIO (Matcher t m) where
  liftIO act = Matcher . WriterT . ReaderT $ \_ ->
    fmap (\r -> (r, Seq.empty)) (liftIO act)

instance MonadTrans (Matcher t) where
  lift act = Matcher . WriterT . ReaderT $ \_ ->
    fmap (\r -> (r, Seq.empty)) (lift act)

instance Monad m => MonadPlus (Matcher t m) where
  mzero = Matcher . WriterT . ReaderT $ \_ -> mzero
  mplus (Matcher (WriterT (ReaderT getX))) (Matcher (WriterT (ReaderT getY)))
    = Matcher . WriterT . ReaderT $ \env ->
    mplus (getX env) (getY env)

-- # Subjects

-- | The current value that is being matched.
getSubject :: Monad m => Matcher t m t
getSubject = Matcher . WriterT . ReaderT $ \(t, _) -> return (t, Seq.empty)

-- | Applies a 'Matcher' to a particular subject.
applyMatcher
  :: Matcher t m a
  -> t
  -> ListT m (a, Seq Message)
applyMatcher (Matcher w) env
  = flip runReaderT (env, nesting0) . runWriterT $ w

atLeastOne
  :: Monad m
  => Matcher t m a
  -> t
  -> m Bool
atLeastOne mr t = do
  ei <- Pipes.next . enumerate $ applyMatcher mr t
  return $ either (const False) (const True) ei


-- | Runs a 'Matcher', but with a subject that is different from that
-- of the current 'Matcher'.  For examples of its use, see 'just' and
-- 'index'.
localSubject
  :: Monad m
  => Matcher t m a
  -> t
  -> Matcher t' m a
localSubject (Matcher (WriterT (ReaderT get))) s
  = Matcher . WriterT . ReaderT $ \ (_, lvl) -> get (s, lvl)

-- # Accepting and rejecting values

-- | Accepts a value; adds an appropriate entry to the log.  You also
-- add an additional annotation (the 'Opinion') to the log.
acceptL :: Monad m => Opinion -> a -> Matcher t m a
acceptL o = acceptW (Just o)

-- | Accepts a value; adds an appropriate entry to the log.  The log
-- will indicate acceptance but it will have no addional annotation.
accept :: Monad m => a -> Matcher t m a
accept = acceptW Nothing

-- | Rejects a value; adds an appropriate entry to the log.
rejectL :: Monad m => Opinion -> Matcher t m a
rejectL o = rejectW (Just o)

-- | Rejects a value; adds an appropriate entry to the log.
reject :: Monad m => Matcher t m a
reject = rejectW Nothing


-- # Logging

-- | Add a notice to the log; it is flagged in the log so it stands
-- out, but it is not an acceptance or rejection.
notice :: Monad m => Opinion -> Matcher t m ()
notice op = do
  l <- currentNesting
  logger (Message l (Just Notice) (Just op))

-- | Adds an informational message to the log.
inform :: Monad m => Opinion -> Matcher t m ()
inform op = do
  l <- currentNesting
  logger (Message l Nothing (Just op))

-- | Runs a 'Matcher' inside of the current 'Matcher'.
nest
  :: Monad m
  => (t' -> m t)
  -- ^ Converts the subject of the current 'Matcher' to the subject of
  -- the 'Matcher' that will run nested.
  -- ^
  -> Matcher t m a
  -- ^ Run this 'Matcher' inside of the current 'Matcher'.
  -> Matcher t' m a
  -- ^
nest = nestW Nothing

-- | Runs a 'Matcher' inside of the current 'Matcher'.
-- Also adds a description to the log.
--
-- For an example of 'nestL' in action, see 'pattern'.
nestL
  :: Monad m
  => Opinion
  -- ^ Add this description to the log.
  -> (t' -> m t)
  -> Matcher t m a
  -- ^ Converts the subject of the current 'Matcher' to the subject of
  -- the 'Matcher' that will run nested.
  -> Matcher t' m a
  -- ^
nestL op = nestW (Just op)

-- | Runs a 'Matcher' inside of the current 'Matcher', and makes a
-- note in the log.  Like 'nestL' but does not allow the type of the
-- nested 'Matcher' to be different.  For an example of its use, see
-- 'just'.
labeled
  :: Monad m
  => Opinion
  -- ^ Add this entry to the log.
  -> Matcher t m a
  -- ^ Run this 'Matcher' inside of the current 'Matcher'
  -> Matcher t m a
  -- ^
labeled op = nestL op return

-- | Like 'predicateW' but does not add an 'Opinion' to the log.  It
-- does, however, make an entry into the log indicating acceptance or
-- rejection.

-- | @predicate f@ creates a 'Matcher' that succeeds and returns the
-- subject if the function @f@ returns 'True'.  It also adds a message
-- to the log, though the message has no additional description.
predicate
  :: Monad m
  => (a -> Bool)
  -> Matcher a m a
predicate f = predicateW (fmap (\b -> (b, Nothing)) f)

-- | Like 'predicate' but also adds additional descriptive information
-- to the log.
predicateL
  :: Monad m
  => (a -> (Bool, Opinion))
  -- ^
  -> Matcher a m a
  -- ^
predicateL f = predicateW (fmap (\(b, o) -> (b, Just o)) f)

-- | Turns a Turtle 'Pattern' into a 'Matcher'.  The 'Matcher' returns
-- every match found by the 'Pattern'.
pattern :: Monad m => Pattern a -> Matcher Text m a
pattern pat = do
  txt <- getSubject
  let op = Opinion
        [ "running text pattern matcher on text: "
        , chunkFromText . X.pack . show $ txt
        ]
  nestL op return
    . F.asum
    . map accept
    . match pat
    $ txt

-- | Always succeeds.
always :: Monad m => Matcher t m ()
always = predicateL (const (True, "always matches"))
  >> return ()

-- | Always fails.
never :: Monad m => Matcher t m a
never = predicateL (const (False, "always fails"))
  >> empty

-- | Succeeds if the subject is 'Just' and the given matcher succeeds
-- on the 'Just' value.
just :: Monad m => Matcher t m a -> Matcher (Maybe t) m a
just m = do
  mayS <- getSubject
  case mayS of
    Nothing -> rejectL "is Nothing"
    Just s -> labeled "is Just" . localSubject m $ s

-- | Succeeds only if the subject is 'Nothing'.
nothing :: Monad m => Matcher (Maybe a) m ()
nothing = do
  tgt <- getSubject
  case tgt of
    Nothing -> acceptL "is Nothing" ()
    _ -> rejectL "is Just"


-- | Runs the given 'Matcher' on every item in the foldable sequence.
-- Returns all matches.
each
  :: (Monad m, Functor f, F.Foldable f)
  => Matcher t m a
  -> Matcher (f t) m a
each mtcr = do
  sq <- getSubject
  labeled "running matcher for each item in sequence"
    . F.asum
    . fmap (localSubject mtcr)
    $ sq


-- | Runs the given 'Matcher' on the 'Seq' item at the given index, if
-- the index is in range.  Returns no matches if the index is out of
-- range.
index
  :: Monad m
  => Int
  -> Matcher t m a
  -> Matcher (Seq t) m a
index idx mr = getSubject >>= f
  where
    f sq
      | idx >= 0 && idx < Seq.length sq =
          labeled (fromString $ "applying Matcher to index " ++ show idx)
                  (localSubject mr (sq `Seq.index` idx))
      | otherwise = rejectL
          (fromString $ "index out of range: " ++ show idx)

-- # Logging

-- | Indicates the current level of nesting.  This is used for logging
-- purposes.  You can increase nesting by using the 'nestW', 'nestL',
-- 'nest', and 'labeled' functions.
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

-- | Use these when describing values that have been accepted or
-- rejected.  The 'acceptW', 'acceptL', 'accept', 'rejectW',
-- 'rejectL', 'reject', and 'notice' functions are easier to use than
-- manipulating this type directly.
data Verdict
  = Accepted
  | Rejected
  | Notice
  deriving (Eq, Ord, Show)

-- | A complete log message.
data Message = Message Nesting (Maybe Verdict) (Maybe Opinion)
  deriving (Eq, Ord, Show)

-- | The current value of nesting.
currentNesting :: Monad m => Matcher t m Nesting
currentNesting = Matcher . WriterT . ReaderT $ \(_, n) ->
  return (n, Seq.empty)


-- | Logs a single message.
logger :: Monad m => Message -> Matcher t m ()
logger msg = Matcher . WriterT . ReaderT $ \_ ->
  return ((), Seq.singleton msg)

-- | Accepts a value; adds an appropriate entry to the log.
acceptW :: Monad m => Maybe Opinion -> a -> Matcher t m a
acceptW op a = do
  l <- currentNesting
  logger (Message l (Just Accepted) op)
  return a

-- | Rejects a value; adds an appropriate entry to the log.
rejectW :: Monad m => Maybe Opinion -> Matcher t m a
rejectW op = do
  l <- currentNesting
  logger (Message l (Just Rejected) op)
  empty

-- | Runs a 'Matcher' inside of the current 'Matcher'.
nestW
  :: Monad m
  => Maybe Opinion
  -- ^ Add to the log, if an 'Opinion' is provided here.  The log
  -- entry can describe the sort of nesting that is occurring.
  -- Otherwise, do not add to the log.
  -> (t' -> m t)
  -- ^ Converts the subject of the current 'Matcher' to the subject of
  -- the 'Matcher' that will run nested.
  -> Matcher t m a
  -- ^ Run this 'Matcher' inside of the current 'Matcher'.
  -> Matcher t' m a
nestW mayOp f (Matcher (WriterT (ReaderT getAct)))
  = Matcher . WriterT . ReaderT $ \(t', n) -> do
    t <- lift $ f t'
    (a, w) <- getAct (t, next n)
    let w' = case mayOp of
          Nothing -> w
          Just op ->
            Message n Nothing (Just ("nesting: " <> op)) <| w
    return (a, w')

-- | @predicate f@ creates a 'Matcher' that will match any value for
-- which @f@ returns True.  The value itself is returned as a witness.
-- Also, an optional 'Opinion' is included in the acceptance or
-- rejection message.
predicateW
  :: Monad m
  => (a -> (Bool, Maybe Opinion))
  -> Matcher a m a
predicateW pd = do
  s <- getSubject
  let (passed, op) = pd s
  if passed
    then acceptW op s
    else rejectW op
