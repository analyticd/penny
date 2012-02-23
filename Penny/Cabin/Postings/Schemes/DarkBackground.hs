module Penny.Cabin.Postings.Schemes.DarkBackground where

import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Colors.Switch as S



evenForeground8 :: CC.Color CC.Color8
evenForeground8 = CC.Default

evenBackground8 :: CC.Color CC.Color8
evenBackground8 = CC.Default

evenForeBack8 :: CC.ForeBack CC.Color8
evenForeBack8 = CC.ForeBack evenForeground8 evenBackground8

evenForeground256 :: CC.Color CC.Color256
evenForeground256 = CC.Default

evenBackground256 :: CC.Color CC.Color256
evenBackground256 = CC.Default

evenForeBack256 :: CC.ForeBack CC.Color256
evenForeBack256 = CC.ForeBack evenForeground256 evenBackground256

evenBaseColorSet :: CC.ColorSet
evenBaseColorSet = CC.ColorSet evenForeBack8 evenForeBack256

evenTextSpec :: CC.TextSpec
evenTextSpec = CC.defaultSpec { CC.colorSet = evenBaseColorSet }

oddTextSpec :: CC.TextSpec
oddTextSpec = S.switchBackground CC.Default
              (CC.Color (CC.color256 235)) evenTextSpec

baseColors :: PC.BaseColors
baseColors = PC.BaseColors evenTextSpec oddTextSpec

