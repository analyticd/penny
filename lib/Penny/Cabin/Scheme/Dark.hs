module Penny.Cabin.Scheme.Dark where

import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Chunk.Switch as S
import qualified Penny.Cabin.Chunk as CC

scheme :: E.Scheme
scheme = E.Labels
  { E.debit = E.EvenAndOdd { E.eoEven = debit evenTextSpec
                           , E.eoOdd = debit oddTextSpec }
  , E.credit = E.EvenAndOdd { E.eoEven = credit evenTextSpec
                            , E.eoOdd = credit oddTextSpec }
  , E.zero = E.EvenAndOdd { E.eoEven = zero evenTextSpec
                          , E.eoOdd = zero oddTextSpec }
  , E.other = E.EvenAndOdd { E.eoEven = evenTextSpec
                           , E.eoOdd = oddTextSpec }
  }

evenTextSpec :: CC.TextSpec
evenTextSpec = CC.defaultTextSpec

oddTextSpec :: CC.TextSpec
oddTextSpec = S.switchBackground CC.color8_b_default
              CC.color256_b_235 evenTextSpec

-- | Debits in 256 colors are orange; in 8 colors, magenta
debit :: CC.TextSpec -> CC.TextSpec
debit = S.switchForeground CC.color8_f_magenta CC.color256_f_208

-- | Credits in 256 colors are cyan; in 8 colors, cyan
credit :: CC.TextSpec -> CC.TextSpec
credit = S.switchForeground CC.color8_f_cyan (CC.color256_f_45)

-- | Zero values are white
zero :: CC.TextSpec -> CC.TextSpec
zero = S.switchForeground CC.color8_f_white (CC.color256_f_15)

