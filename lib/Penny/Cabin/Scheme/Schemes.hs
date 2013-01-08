-- | Some schemes you can use.

module Penny.Cabin.Scheme.Schemes where

import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Chunk.Switch as Sw

-- | The light color scheme. You can change various values below to
-- affect the color scheme.
light :: E.Scheme
light = E.Scheme "light" "for light background terminals"
              lightLabels

lightLabels :: E.Labels (E.EvenAndOdd C.TextSpec)
lightLabels = E.Labels
  { E.debit = E.EvenAndOdd { E.eoEven = lightDebit lightEvenTextSpec
                           , E.eoOdd = lightDebit lightOddTextSpec }
  , E.credit = E.EvenAndOdd { E.eoEven = lightCredit lightEvenTextSpec
                            , E.eoOdd = lightCredit lightOddTextSpec }
  , E.zero = E.EvenAndOdd { E.eoEven = lightZero lightEvenTextSpec
                          , E.eoOdd = lightZero lightOddTextSpec }
  , E.other = E.EvenAndOdd { E.eoEven = lightEvenTextSpec
                           , E.eoOdd = lightOddTextSpec }
  }

lightEvenTextSpec :: C.TextSpec
lightEvenTextSpec = C.defaultTextSpec

lightOddTextSpec :: C.TextSpec
lightOddTextSpec = Sw.switchBackground C.color8_b_default
                   C.color256_b_255 lightEvenTextSpec

lightDebit :: C.TextSpec -> C.TextSpec
lightDebit = Sw.switchForeground C.color8_f_magenta C.color256_f_52

lightCredit :: C.TextSpec -> C.TextSpec
lightCredit = Sw.switchForeground C.color8_f_cyan C.color256_f_21

lightZero :: C.TextSpec -> C.TextSpec
lightZero = Sw.switchForeground C.color8_f_black C.color256_f_0

-- | The dark color scheme. You can change various values below to
-- affect the color scheme.
dark :: E.Scheme
dark = E.Scheme "dark" "for dark background terminals"
              darkLabels

darkLabels :: E.Labels (E.EvenAndOdd C.TextSpec)
darkLabels = E.Labels
  { E.debit = E.EvenAndOdd { E.eoEven = darkDebit darkEvenTextSpec
                           , E.eoOdd = darkDebit darkOddTextSpec }
  , E.credit = E.EvenAndOdd { E.eoEven = darkCredit darkEvenTextSpec
                            , E.eoOdd = darkCredit darkOddTextSpec }
  , E.zero = E.EvenAndOdd { E.eoEven = darkZero darkEvenTextSpec
                          , E.eoOdd = darkZero darkOddTextSpec }
  , E.other = E.EvenAndOdd { E.eoEven = darkEvenTextSpec
                           , E.eoOdd = darkOddTextSpec }
  }

darkEvenTextSpec :: C.TextSpec
darkEvenTextSpec = C.defaultTextSpec

darkOddTextSpec :: C.TextSpec
darkOddTextSpec = Sw.switchBackground C.color8_b_default
                   C.color256_b_235 darkEvenTextSpec

darkDebit :: C.TextSpec -> C.TextSpec
darkDebit = Sw.switchForeground C.color8_f_magenta C.color256_f_208

darkCredit :: C.TextSpec -> C.TextSpec
darkCredit = Sw.switchForeground C.color8_f_cyan C.color256_f_45

darkZero :: C.TextSpec -> C.TextSpec
darkZero = Sw.switchForeground C.color8_f_white C.color256_f_15

-- | Plain scheme has no colors at all.
plain :: E.Scheme
plain = E.Scheme "plain" "uses default terminal colors"
              plainLabels

plainLabels :: E.Labels (E.EvenAndOdd C.TextSpec)
plainLabels = E.Labels
  { E.debit = E.EvenAndOdd C.defaultTextSpec C.defaultTextSpec
  , E.credit = E.EvenAndOdd C.defaultTextSpec C.defaultTextSpec
  , E.zero = E.EvenAndOdd C.defaultTextSpec C.defaultTextSpec
  , E.other = E.EvenAndOdd C.defaultTextSpec C.defaultTextSpec
  }

