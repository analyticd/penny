-- | Options applicable to multiple Cabin reports.

module Penny.Cabin.Options where

import qualified Penny.Cabin.Chunk as C
import qualified Penny.Shield as S

-- | The user's color preference.
data ColorPref = Pref0 | Pref8 | Pref256 | PrefAuto
               deriving Show

-- | The maximum number of colors that can be displayed. If not a TTY,
-- no colors. Otherwise, Examines TERM. If it is @xterm-256color@,
-- then 256 colors; otherwise, assumes 8 colors are available.
maxCapableColors :: S.Runtime -> C.Colors
maxCapableColors r = case S.output r of
  S.NotTTY -> C.Colors0
  S.IsTTY ->
    case lookup "TERM" (S.environment r) of
      Nothing -> C.Colors8
      (Just t) -> if t == "xterm-256color"
                  then C.Colors256
                  else C.Colors8

-- | Given the user's color preference, and the runtime, calculate how
-- many colors to use. Respects the user's preference, but if the
-- preference is to set colors automatically, then uses information
-- from the runtime to display the maximum number of possible colors.
autoColors :: ColorPref -> S.Runtime -> C.Colors
autoColors c r = case c of
  Pref0 -> C.Colors0
  Pref8 -> C.Colors8
  Pref256 -> C.Colors256
  PrefAuto -> maxCapableColors r

-- | Whether to show zero balances in reports.
newtype ShowZeroBalances =
  ShowZeroBalances { unShowZeroBalances :: Bool }
  deriving (Show, Eq)

-- | Converts an ordering to a descending order.
descending :: (a -> a -> Ordering)
              -> a -> a -> Ordering
descending f x y = case f x y of
  LT -> GT
  GT -> LT
  EQ -> EQ
