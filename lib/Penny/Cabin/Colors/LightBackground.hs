-- | The default colors; the user may override.
module Penny.Cabin.Colors.LightBackground where

import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Chunk as CC
import qualified Penny.Cabin.Chunk.Switch as S

baseColors :: PC.BaseColors
baseColors = PC.BaseColors evenTextSpec oddTextSpec

drCrColors :: PC.DrCrColors
drCrColors = PC.DrCrColors { PC.evenDebit = debit evenTextSpec
                           , PC.evenCredit = credit evenTextSpec
                           , PC.oddDebit = debit oddTextSpec
                           , PC.oddCredit = credit oddTextSpec
                           , PC.evenZero = zero evenTextSpec
                           , PC.oddZero = zero oddTextSpec }

evenTextSpec :: CC.TextSpec
evenTextSpec = CC.defaultSpec

oddTextSpec :: CC.TextSpec
oddTextSpec = S.switchBackground CC.defaultColor
              (CC.color256_247) evenTextSpec
              
debit :: CC.TextSpec -> CC.TextSpec
debit = S.switchForeground CC.magenta (CC.color256_13)

credit :: CC.TextSpec -> CC.TextSpec
credit = S.switchForeground CC.cyan (CC.color256_14)

zero :: CC.TextSpec -> CC.TextSpec
zero = S.switchForeground CC.white (CC.color256_15)


