{-# LANGUAGE RankNTypes #-}
-- | Miscellaneous parsing utilities.

module Penny.Copper.Util where

import Data.Text (Text)
import qualified Text.Earley as E

data ParseFailureReason a
  = NoResults
  | ManyResults a [a]
  deriving Show

data ParseFailure a = ParseFailure
  { reason :: ParseFailureReason a
  , input :: Text
  , report :: E.Report String Text
  } deriving Show

-- | Runs an Earley parser.  If there is a single result, return it.
-- Otherwise, returns an error message.  Returns an error message
-- (rather than throwing an exception) for an ambiguous grammar; I'm
-- not sure this is the correct design decision.
parse
  :: (forall r. E.Grammar r (E.Prod r String Char a))
  -- ^ Earley grammar with appropriate production
  -> Text
  -- ^ Parse this text
  -> Either (ParseFailure a) a
parse grammar txt = case results of
  [] -> Left (ParseFailure NoResults txt report)
  x:[] -> Right x
  x:xs -> Left (ParseFailure (ManyResults x xs) txt report)
  where
    (results, report) = E.fullParses (E.parser grammar) txt
