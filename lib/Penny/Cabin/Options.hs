-- | Options applicable to multiple Cabin reports.

module Penny.Cabin.Options where

import qualified Penny.Cabin.Chunk as C
import qualified Penny.Shield as S

-- | The user's color preference.
data ColorPref = Pref0 | Pref8 | Pref256 | PrefAuto
               deriving Show

maxCapableColors :: S.Runtime -> C.Colors
maxCapableColors r = case S.output r of
  S.NotTTY -> C.Colors0
  S.IsTTY ->
    case lookup "TERM" (S.environment r) of
      Nothing -> C.Colors8
      (Just t) -> if t == "xterm-256color"
                  then C.Colors256
                  else C.Colors8

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

