-- | Some schemes you can use.

module Penny.Cabin.Scheme.Schemes where

import Data.Monoid ( (<>) )
import qualified Penny.Cabin.Scheme as E
import qualified System.Console.Rainbow as R

-- | The light color scheme. You can change various values below to
-- affect the color scheme.
light :: E.Scheme
light = E.Scheme "light" "for light background terminals"
              lightLabels

lightLabels :: E.Labels (E.EvenAndOdd (R.Chunk -> R.Chunk))
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

lightEvenTextSpec :: R.Chunk -> R.Chunk
lightEvenTextSpec = id

lightOddTextSpec :: R.Chunk -> R.Chunk
lightOddTextSpec = (<> (R.c8_b_default <> R.c256_b_255))

lightDebit :: (R.Chunk -> R.Chunk) -> R.Chunk -> R.Chunk
lightDebit f c = f c <> R.c8_f_magenta <> R.c256_f_52

lightCredit :: (R.Chunk -> R.Chunk) -> R.Chunk -> R.Chunk
lightCredit f c = f c <> R.c8_f_cyan <> R.c256_f_21

lightZero :: (R.Chunk -> R.Chunk) -> R.Chunk -> R.Chunk
lightZero f c = f c <> R.c8_f_black <> R.c256_f_0

-- | The dark color scheme. You can change various values below to
-- affect the color scheme.
dark :: E.Scheme
dark = E.Scheme "dark" "for dark background terminals"
              darkLabels

darkLabels :: E.Labels (E.EvenAndOdd (R.Chunk -> R.Chunk))
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

darkEvenTextSpec :: R.Chunk -> R.Chunk
darkEvenTextSpec = id

darkOddTextSpec :: R.Chunk -> R.Chunk
darkOddTextSpec = (<> (R.c8_b_default <> R.c256_b_235))

darkDebit :: (R.Chunk -> R.Chunk) -> R.Chunk -> R.Chunk
darkDebit f c = f c <> R.c8_f_magenta <> R.c256_f_208

darkCredit :: (R.Chunk -> R.Chunk) -> R.Chunk -> R.Chunk
darkCredit f c = f c <> R.c8_f_cyan <> R.c256_f_45

darkZero :: (R.Chunk -> R.Chunk) -> R.Chunk -> R.Chunk
darkZero f c = f c <> R.c8_f_white <> R.c256_f_15

-- | Plain scheme has no colors at all.
plain :: E.Scheme
plain = E.Scheme "plain" "uses default terminal colors"
              plainLabels

plainLabels :: E.Labels (E.EvenAndOdd (R.Chunk -> R.Chunk))
plainLabels = E.Labels
  { E.debit = E.EvenAndOdd id id
  , E.credit = E.EvenAndOdd id id
  , E.zero = E.EvenAndOdd id id
  , E.other = E.EvenAndOdd id id
  }

