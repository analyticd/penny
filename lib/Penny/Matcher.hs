{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Matchers.
--
-- Here are the core components of 'Matcher' and matchers for prelude
-- types.  The idea is that this module could eventually be extracted
-- from Penny and be a package that Penny depends on.
module Penny.Matcher where

{-
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
-}

import Control.Monad
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pipes hiding (next, each)
import qualified Pipes
import qualified Data.Foldable as F
import Data.Text (Text)
import Turtle.Pattern
import Control.Lens hiding (Level, each, index)
import Prelude hiding (filter)

true :: MonadPlus m => Bool -> m ()
true b | b = return ()
       | otherwise = mzero

false :: MonadPlus m => Bool -> m ()
false b | not b = return ()
        | otherwise = mzero

just :: MonadPlus m => Maybe a -> m a
just = maybe mzero return

nothing :: MonadPlus m => Maybe a -> m ()
nothing = maybe (return ()) (const mzero)

left :: MonadPlus m => Either a b -> m a
left = either return (const mzero)

right :: MonadPlus m => Either a b -> m b
right = either (const mzero) return

-- | Runs the given 'Matcher' on every item in the foldable sequence.
-- Returns all matches.
each
  :: (MonadPlus m, Functor f, Foldable f)
  => (a -> m b)
  -> (f a -> m b)
each mtcr = F.msum . fmap (mtcr $)

index
  :: MonadPlus m
  => Int
  -> Seq s
  -> m s
index idx sq
  | idx >= 0 && idx < Seq.length sq = return (sq `Seq.index` idx)
  | otherwise = mzero

-- | Creates a 'Matcher' from a Turtle 'Pattern'.
pattern
  :: MonadPlus m
  => Pattern a
  -> Text
  -> m a
pattern pat = F.msum . fmap return . match pat

-- | Filters a 'Seq' using a 'Matcher'.
filter
  :: Monad m
  => (a -> ListT m b)
  -- ^ Filter using this matcher.  The subject is included in the
  -- result if the matcher returns a single value of any type.
  -> Seq a
  -- ^ Filter this sequence
  -> m (Seq a)
filter mtcr = F.foldlM f Seq.empty
  where
    f mtchs subj = do
      ei <- Pipes.next . enumerate $ mtcr subj
      return $ case ei of
        Left _ -> mtchs
        Right _ -> mtchs |> subj
