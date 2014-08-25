module Penny.Copper.Unpolar.Parse where

import Control.Applicative
import Control.Monad hiding (sequence)
import Text.Parsec.Text (Parser)
import Text.Parsec (char, choice)
import Penny.Copper.Unpolar.Tree
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Prelude hiding (sequence)
import Data.Sums
import Penny.Numbers.Concrete (NovDecs(..))
import Deka.Native.Abstract
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Unpolar hiding (Zeroes)

sequence :: Parser a -> Parser (Seq a)
sequence p = fmap S.fromList $ many p

zero :: Parser Zero
zero = Zero <$ char '0'

zeroes :: Parser Zeroes
zeroes = Zeroes <$> zero <*> sequence zero

novem :: Parser Novem
novem = choice
  [ D1 <$ char '1'
  , D2 <$ char '2'
  , D3 <$ char '3'
  , D4 <$ char '4'
  , D5 <$ char '5'
  , D6 <$ char '6'
  , D7 <$ char '7'
  , D8 <$ char '8'
  , D9 <$ char '9'
  ]

decem :: Parser Decem
decem = choice
  [ D0 <$ char '0'
  , Nonem <$> novem
  ]

novDecs :: Parser NovDecs
novDecs = NovDecs <$> novem <*> sequence decem

decDecs :: Parser DecDecs
decDecs = DecDecs <$> decem <*> sequence decem

-- # Radix

radixPeriod :: Parser (Radix Period)
radixPeriod = radPeriod <$ char '.'

radixComma :: Parser (Radix Comma)
radixComma = radComma <$ char ','

-- # Groups

group :: Parser p -> Parser (Grouper r) -> Parser (Group r p)
group p r = Group <$> r <*> p

groupMaybeZnd
  :: Parser (Grouper r)
  -> Parser (Group r (Maybe Zeroes, NovDecs))
groupMaybeZnd = group $ (,) <$> optional zeroes <*> novDecs

groupDecDecs
  :: Parser (Grouper r)
  -> Parser (Group r DecDecs)
groupDecDecs = group decDecs

groupZeroes
  :: Parser (Grouper r)
  -> Parser (Group r Zeroes)
groupZeroes = group zeroes

grouperPeriod :: Parser (Grouper Period)
grouperPeriod = choice
  [ Space <$ char ' '
  , Thin <$ char '\x2009'
  , Under <$ char '_'
  , Unique Comma <$ char ','
  ]

grouperComma :: Parser (Grouper Comma)
grouperComma = choice
  [ Space <$ char ' '
  , Thin <$ char '\x2009'
  , Under <$ char '_'
  , Unique Period <$ char '.'
  ]

-- # Tree components

aRZNext
  :: Parser (Group r (Maybe Zeroes, NovDecs))
  -> Parser (Group r DecDecs)
  -> Parser (ARZNext r)
aRZNext pnd pdd = choice
  [ liftM2 ARZNNovDecs novDecs (sequence pdd)
  , liftM2 ARZNGroups pnd (sequence pdd)
  , return ARZNEnd
  ]

aRZeroes
  :: Parser (Group r (Maybe Zeroes, NovDecs))
  -> Parser (Group r Zeroes)
  -> Parser (Group r DecDecs)
  -> Parser (ARZeroes r)
aRZeroes pnd pz pdd = choice
  [ liftM3 ARZGroupsZ pz (sequence pz) (aRZNext pnd pdd)
  , liftM2 ARZNovDecs novDecs (sequence pdd)
  , return ARZEnd
  ]

afterRad
  :: Parser (Group r (Maybe Zeroes, NovDecs))
  -> Parser (Group r Zeroes)
  -> Parser (Group r DecDecs)
  -> Parser (AfterRad r)
afterRad pnd pz pdd = choice
  [ liftM2 ARZeroes zeroes (aRZeroes pnd pz pdd)
  , liftM2 ARNovDecs novDecs (sequence pdd)
  , return AREnd
  ]

zF
  :: Parser (Radix r)
  -> Parser (Group r (Maybe Zeroes, NovDecs))
  -> Parser (Group r Zeroes)
  -> Parser (Group r DecDecs)
  -> Parser (ZF r)
zF rad pnd pz pdd = choice
  [ liftM2 ZFRadix rad (afterRad pnd pz pdd)
  , return ZFEnd
  ]

zeroFirst
  :: Parser (Radix r)
  -> Parser (Group r (Maybe Zeroes, NovDecs))
  -> Parser (Group r Zeroes)
  -> Parser (Group r DecDecs)
  -> Parser (ZeroFirst r)
zeroFirst rad pnd pz pdd = liftM2 ZeroFirst zero (zF rad pnd pz pdd)

nDF1RadixDigits
  :: Parser (Group r DecDecs)
  -> Parser (NDF1RadixDigits r)
nDF1RadixDigits pd = choice
  [ liftM2 NDF1RadixDigitsGroups pd (sequence pd)
  , return NDF1RadixDigitsEnd
  ]

nDF1Radix
  :: Parser (Group r DecDecs)
  -> Parser (NDF1Radix r)
nDF1Radix pd = choice
  [ liftM2 NDF1RadixDigits decDecs (nDF1RadixDigits pd)
  , return NDF1RadixEnd
  ]

nDF1Group
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (NDF1Group r)
nDF1Group rad pd = choice
  [ liftM2 NDF1GroupRadix rad (nDF1Radix pd)
  , return NDF1GroupEnd
  ]

nDF1
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (NDF1 r)
nDF1 rad pd = choice
  [ liftM2 NDF1Radix rad (nDF1Radix pd)
  , liftM3 NDF1Group pd (sequence pd) (nDF1Group rad pd)
  , return NDF1End
  ]

novDecsFirst
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (NovDecsFirst r)
novDecsFirst rad pd = liftM2 NovDecsFirst novDecs (nDF1 rad pd)

radixFirst
  :: Parser (Radix r)
  -> Parser (Group r (Maybe Zeroes, NovDecs))
  -> Parser (Group r Zeroes)
  -> Parser (Group r DecDecs)
  -> Parser (RadixFirst r)
radixFirst pr pnd pz pd = liftM2 RadixFirst pr (afterRad pnd pz pd)

start
  :: Parser (Radix r)
  -> Parser (Group r (Maybe Zeroes, NovDecs))
  -> Parser (Group r Zeroes)
  -> Parser (Group r DecDecs)
  -> Parser (Start r)
start r m z d = fmap Start $ choice
  [ fmap S3a $ novDecsFirst r d
  , fmap S3b $ zeroFirst r m z d
  , fmap S3c $ radixFirst r m z d
  ]

startPeriod :: Parser (Start Period)
startPeriod =
  start radixPeriod (groupMaybeZnd grouperPeriod)
        (groupZeroes grouperPeriod) (groupDecDecs grouperPeriod)

startComma :: Parser (Start Comma)
startComma =
  start radixComma (groupMaybeZnd grouperComma)
        (groupZeroes grouperComma) (groupDecDecs grouperComma)
