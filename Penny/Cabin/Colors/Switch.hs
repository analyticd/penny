module Penny.Cabin.Colors.Switch where

import qualified Penny.Cabin.Colors as C

-- | Switch the foreground colors for new ones.
switchForeground ::
  C.Color C.Color8
  -> C.Color C.Color256
  -> C.TextSpec
  -> C.TextSpec
switchForeground c8 c256 ts = ts' where
  ts' = C.TextSpec s8' s256'
  s8' = (C.style8 ts) { C.foreground = c8 }
  s256' = (C.style256 ts) { C.foreground = c256 }

-- | Switch the background colors for new ones.
switchBackground ::
  C.Color C.Color8
  -> C.Color C.Color256
  -> C.TextSpec
  -> C.TextSpec
switchBackground c8 c256 ts = ts' where
  ts' = C.TextSpec s8' s256'
  s8' = (C.style8 ts) { C.background = c8 }
  s256' = (C.style256 ts) { C.background = c256 }

