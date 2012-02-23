module Penny.Cabin.Colors.Switch where

import qualified Penny.Cabin.Colors as C

-- | Switch the foreground colors for new ones.
switchForeground ::
  C.Color C.Color8
  -> C.Color C.Color256
  -> C.TextSpec
  -> C.TextSpec
switchForeground c8 c256 ts = ts { C.colorSet = cs' } where
  cs = C.colorSet ts
  cs' = C.ColorSet fb8' fb256'
  fb8 = C.colorSet8 cs
  fb256 = C.colorSet256 cs
  fb8' = fb8 { C.foreground = c8 }
  fb256' = fb256 { C.foreground = c256 }

-- | Switch the background colors for new ones.
switchBackground ::
  C.Color C.Color8
  -> C.Color C.Color256
  -> C.TextSpec
  -> C.TextSpec
switchBackground c8 c256 ts = ts { C.colorSet = cs' } where
  cs = C.colorSet ts
  cs' = C.ColorSet fb8' fb256'
  fb8 = C.colorSet8 cs
  fb256 = C.colorSet256 cs
  fb8' = fb8 { C.background = c8 }
  fb256' = fb256 { C.background = c256 }

