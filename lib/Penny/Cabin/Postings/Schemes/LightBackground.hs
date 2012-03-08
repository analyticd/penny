module Penny.Cabin.Postings.Schemes.LightBackground where

import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Colors.Switch as S

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
              (CC.color256 247) evenTextSpec
              
debit :: CC.TextSpec -> CC.TextSpec
debit = S.switchForeground CC.magenta (CC.color256 13)

credit :: CC.TextSpec -> CC.TextSpec
credit = S.switchForeground CC.cyan (CC.color256 14)

zero :: CC.TextSpec -> CC.TextSpec
zero = S.switchForeground CC.white (CC.color256 15)


