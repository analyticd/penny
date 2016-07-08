-- | Quasi quoters to make it easier to enter some values that are
-- tedious to express in Haskell source.
module Penny.Quasi where

import Data.Data (Data)
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified Language.Haskell.TH as T
import qualified Language.Haskell.TH.Quote as TQ

import Penny.Copper (runParser)
import Penny.Copper.Decopperize (dDate, dTime, dNilOrBrimRadPer)
import Penny.Copper.Productions
import Penny.Copper.EarleyGrammar
import Penny.Decimal

liftData :: Data a => a -> T.Q T.Exp
liftData = TQ.dataToExpQ (const Nothing)

expOnly :: (String -> T.Q T.Exp) -> TQ.QuasiQuoter
expOnly q = TQ.QuasiQuoter
  { TQ.quoteExp = q
  , TQ.quotePat = undefined
  , TQ.quoteType = undefined
  , TQ.quoteDec = undefined
  }

-- | Quasi quoter for dates.  Enter as YYYY-MM-DD.  The resulting
-- expression has type 'Time.Day'.
qDay :: TQ.QuasiQuoter
qDay = expOnly $ \s ->
  case runParser (fmap a'Date earleyGrammar) (X.strip . X.pack $ s) of
    Left _ -> fail $ "invalid date: " ++ s
    Right d -> liftData . dDate $ d

-- | Quasi quoter for time of day.  Enter as HH:MM:SS, where the
-- seconds are optional.  The resulting expression has type
-- 'Time.TimeOfDay' The resulting expression has type
-- 'Time.TimeOfDay'.
qTime :: TQ.QuasiQuoter
qTime = expOnly $ \s ->
  case runParser (fmap a'Time earleyGrammar) (X.strip . X.pack $ s) of
    Left _ -> fail $ "invalid time of day: " ++ s
    Right d -> liftData . dTime $ d

-- | Quasi quoter for unsigned decimal numbers.  The resulting
-- expression has type 'Exponential' 'NonNegative'.  The string can
-- have grouping characters, but the radix point mus always be a
-- period.
pUnsigned :: TQ.QuasiQuoter
pUnsigned = expOnly $ \s ->
  case runParser (fmap a'NilOrBrimRadPer earleyGrammar) (X.strip . X.pack $ s) of
    Left _ -> fail $ "invalid unsigned number: " ++ s
    Right d -> liftData . dNilOrBrimRadPer $ d
