module Penny.Cabin.Postings.Schemes.DarkBackground where

import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Colors.Switch as S

evenTextSpec :: CC.TextSpec
evenTextSpec = CC.defaultSpec

oddTextSpec :: CC.TextSpec
oddTextSpec = S.switchBackground CC.Default
              (CC.Color (CC.color256 235)) evenTextSpec
              
baseColors :: PC.BaseColors
baseColors = PC.BaseColors evenTextSpec oddTextSpec

evenDebit :: CC.TextSpec
evenDebit = S.switchForeground (CC.Color CC.Magenta)
            (CC.Color (CC.color256 13)) evenTextSpec
