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

import Control.Monad
import Control.Applicative
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pipes hiding (next, each)
import qualified Pipes
import qualified Data.Foldable as F
import Data.Text (Text)
import Turtle.Pattern
import Control.Lens hiding (Level, each, index)
import Prelude hiding (filter)

just :: Alternative m => Maybe a -> m a
just = maybe empty pure

nothing :: Alternative m => Maybe a -> m ()
nothing = maybe (pure ()) (const empty)

left :: Alternative m => Either a b -> m a
left = either pure (const empty)

right :: Alternative m => Either a b -> m b
right = either (const empty) pure

-- | Runs the given 'Matcher' on every item in the foldable sequence.
-- Returns all matches.
each
  :: (Alternative m, Functor f, Foldable f)
  => (a -> m b)
  -> (f a -> m b)
each mtcr = F.asum . fmap (mtcr $)

index
  :: Alternative m
  => Int
  -> Seq s
  -> m s
index idx sq
  | idx >= 0 && idx < Seq.length sq = pure (sq `Seq.index` idx)
  | otherwise = empty

-- | Creates a 'Matcher' from a Turtle 'Pattern'.
pattern
  :: Alternative m
  => Pattern a
  -> Text
  -> m a
pattern pat = F.asum . fmap pure . match pat

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
