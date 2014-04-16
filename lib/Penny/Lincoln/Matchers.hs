-- | Type synonyms for functions dealing with text matching.

module Penny.Lincoln.Matchers where

import qualified Data.Text as X
import qualified Text.Matchers as MT
import qualified Data.Prednote as P

-- | A function that makes Matchers.
type Factory
  = MT.CaseSensitive
  -- ^ Will this matcher be case sensitive?

  -> X.Text
  -- ^ The pattern to use when testing for a match. For example, this
  -- might be a regular expression, or simply the text to be matched.

  -> Either X.Text (P.Predbox X.Text)
  -- ^ Sometimes producing a matcher might fail; for example, the user
  -- might have supplied a bad pattern. If so, an exception is
  -- returned. On success, a matcher is returned.
