-- | Quasi quoters to make it easier to enter some values that are
-- tedious to express in Haskell source.
module Penny.Quasi where

import Data.Data (Data)
import qualified Data.Text as X
import qualified Language.Haskell.TH as T
import qualified Language.Haskell.TH.Quote as TQ

import Penny.Copper (parseProduction)
import Penny.Copper.Decopperize (dDate, dTime, dNilOrBrimRadPer)
import Penny.Copper.Productions

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
  case parseProduction a'Date (X.strip . X.pack $ s) of
    Left _ -> fail $ "invalid date: " ++ s
    Right d -> liftData . dDate $ d

-- | Quasi quoter for time of day.  Enter as HH:MM:SS, where the
-- seconds are optional.  The resulting expression has type
-- 'Time.TimeOfDay' The resulting expression has type
-- 'Time.TimeOfDay'.
qTime :: TQ.QuasiQuoter
qTime = expOnly $ \s ->
  case parseProduction a'Time (X.strip . X.pack $ s) of
    Left _ -> fail $ "invalid time of day: " ++ s
    Right d -> liftData . dTime $ d

-- | Quasi quoter for unsigned decimal numbers.  The resulting
-- expression has type 'Exponential' 'NonNegative'.  The string can
-- have grouping characters, but the radix point mus always be a
-- period.
qUnsigned :: TQ.QuasiQuoter
qUnsigned = expOnly $ \s ->
  case parseProduction a'NilOrBrimRadPer (X.strip . X.pack $ s) of
    Left _ -> fail $ "invalid unsigned number: " ++ s
    Right d -> liftData . dNilOrBrimRadPer $ d
