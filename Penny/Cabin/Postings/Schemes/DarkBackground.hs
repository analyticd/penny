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

oddForeground8 :: CC.Color CC.Color8
oddForeground8 = CC.Default

oddBackground8 :: CC.Color CC.Color8
oddBackground8 = CC.Default

oddForeBack8 :: CC.ForeBack CC.Color8
oddForeBack8 = CC.ForeBack oddForeground8 oddBackground8

oddForeground256 :: CC.Color CC.Color256
oddForeground256 = CC.Default

-- | Dark grey
oddBackground256 :: CC.Color CC.Color256
oddBackground256 = CC.Color (CC.color256 235)

oddForeBack256 :: CC.ForeBack CC.Color256
oddForeBack256 = CC.ForeBack oddForeground256 oddBackground256

evenBaseColorSet :: CC.ColorSet
evenBaseColorSet = CC.ColorSet evenForeBack8 evenForeBack256

oddBaseColorSet :: CC.ColorSet
oddBaseColorSet = CC.ColorSet oddForeBack8 oddForeBack256

evenTextSpec :: CC.TextSpec
evenTextSpec = CC.defaultSpec { CC.colorSet = evenBaseColorSet }

oddTextSpec :: CC.TextSpec
oddTextSpec = CC.defaultSpec { CC.colorSet = oddBaseColorSet }

baseColors :: PC.BaseColors
baseColors = PC.BaseColors evenTextSpec oddTextSpec
