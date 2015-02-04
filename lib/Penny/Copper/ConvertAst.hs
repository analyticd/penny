module Penny.Copper.ConvertAst where

import Penny.Lincoln
import Penny.Copper.Ast
import Penny.Copper.Terminals
import Data.Text (Text)
import qualified Data.Text as X
import Data.Maybe (mapMaybe, isJust)

decimalPlace :: (Digit a, Integral b) => Int -> a -> b
decimalPlace pl dig = digitToInt dig * 10 ^ pl

c'Int'DigitsFour :: Integral a => DigitsFour -> a
c'Int'DigitsFour (DigitsFour d3 d2 d1 d0)
  = decimalPlace 3 d3
  + decimalPlace 2 d2
  + decimalPlace 1 d1
  + decimalPlace 0 d0

c'Int'Digits1or2 :: Integral a => Digits1or2 -> a
c'Int'Digits1or2 (Digits1or2 l mayR) = case mayR of
  Nothing -> decimalPlace 0 l
  Just r -> decimalPlace 1 l + decimalPlace 0 r

-- | Bad Date
data DateE = DateE DateA
  deriving (Eq, Ord, Show)

c'Date'DateA :: DateA -> Either DateE Date
c'Date'DateA a@(DateA _ y _ m _ d)
  = maybe (Left $ DateE a) Right
  $ fromGregorian (c'Int'DigitsFour y)
                  (c'Int'Digits1or2 m)
                  (c'Int'Digits1or2 d)

c'Hours'HoursA :: HoursA -> Hours
c'Hours'HoursA x = case x of
  H0to19a m d -> H0to19 m d
  H20to23a _ d3 -> H20to23 d3

c'Time'TimeA :: TimeA -> Time
c'Time'TimeA (TimeA hA _ m mayS) = Time (c'Hours'HoursA hA) m s
  where
    s = case mayS of
      Nothing -> Seconds (ZeroTo59 Nothing D9z'0)
      Just (_, x) -> x

c'MaybeChar'EscSeq :: EscSeq -> Maybe Char
c'MaybeChar'EscSeq (EscSeq _ py) = case py of
  EscBackslash -> Just '\\'
  EscNewline -> Just '\n'
  EscQuote -> Just '"'
  EscGap _ _ -> Nothing

c'MaybeChar'QuotedChar :: QuotedChar -> Maybe Char
c'MaybeChar'QuotedChar (QuotedChar ei)
  = either (Just . termToChar) c'MaybeChar'EscSeq ei

c'Text'UnquotedString :: UnquotedString -> Text
c'Text'UnquotedString (UnquotedString digs nonDig ls)
  = X.pack
  . (map c'Char'D9z digs ++)
  . (termToChar nonDig :)
  $ map (either termToChar c'Char'D9z) ls

c'Commodity'UnquotedCommodityOnLeft
  :: UnquotedCommodityOnLeft
  -> Commodity
c'Commodity'UnquotedCommodityOnLeft (UnquotedCommodityOnLeft d1 ds)
  = Commodity
  . X.pack
  . (termToChar d1 :)
  $ map termToChar ds

c'Commodity'UnquotedCommodityOnRight
  :: UnquotedCommodityOnRight
  -> Commodity
c'Commodity'UnquotedCommodityOnRight (UnquotedCommodityOnRight d1 ds)
  = Commodity
  . X.pack
  . (termToChar d1 :)
  $ map termToChar ds

c'Commodity'QuotedCommodity
  :: QuotedCommodity
  -> Commodity
c'Commodity'QuotedCommodity (QuotedCommodity q)
  = Commodity . c'Text'QuotedString $ q

c'Text'QuotedString :: QuotedString -> Text
c'Text'QuotedString (QuotedString _ ls _)
  = X.pack
  . mapMaybe c'MaybeChar'QuotedChar
  $ ls

c'Char'NonEscapedChar :: NonEscapedChar -> Char
c'Char'NonEscapedChar = termToChar

c'Commodity'CommodityOnLeftA :: CommodityOnLeftA -> Commodity
c'Commodity'CommodityOnLeftA (CommodityOnLeftA ei)
  = either c'Commodity'UnquotedCommodityOnLeft
           c'Commodity'QuotedCommodity ei

c'Commodity'CommodityOnRightA :: CommodityOnRightA -> Commodity
c'Commodity'CommodityOnRightA (CommodityOnRightA ei)
  = either c'Commodity'UnquotedCommodityOnRight
           c'Commodity'QuotedCommodity ei

c'Commodity'CommodityA :: CommodityA -> Commodity
c'Commodity'CommodityA (CommodityA ei)
  = either c'Commodity'UnquotedCommodityOnRight
           c'Commodity'QuotedCommodity ei

qtyRepAnyRadix :: NonNeutral -> Side -> QtyRepAnyRadix
qtyRepAnyRadix nn s = nilOrBrimScalarAnyRadixToQty s nbs
  where
    nbs = case nn of
      NonNeutralRadCom _ rc -> NilOrBrimScalarAnyRadix
        . Left . NilOrBrimScalar . Right $ rc
      NonNeutralRadPer rp -> NilOrBrimScalarAnyRadix
        . Right . NilOrBrimScalar . Right $ rp

c'RepNonNeutralNoSide'NonNeutral :: NonNeutral -> RepNonNeutralNoSide
c'RepNonNeutralNoSide'NonNeutral x = RepNonNeutralNoSide $ case x of
  NonNeutralRadCom _ b -> Left b
  NonNeutralRadPer p -> Right p

repAnyRadixFromNonNeutral :: Side -> NonNeutral -> QtyRepAnyRadix
repAnyRadixFromNonNeutral s nn = QtyRepAnyRadix ei
  where
    ei = case nn of
      NonNeutralRadCom _ br -> Left . QtyRep
        . NilOrBrimPolar . OffCenter br $ s
      NonNeutralRadPer br -> Right . QtyRep
        . NilOrBrimPolar . OffCenter br $ s

c'Trio'TrioA :: TrioA -> Trio
c'Trio'TrioA trio = case trio of
  QcCyOnLeftA (Fs side _) (Fs cy cySpc) nn ->
    QC (qtyRepAnyRadix nn side)
       (c'Commodity'CommodityOnLeftA cy)
       (arrangement cySpc CommodityOnLeft)

  QcCyOnRightA (Fs side _) (Fs nn cySpc) cyOnRight ->
    QC (qtyRepAnyRadix nn side)
       (c'Commodity'CommodityOnRightA cyOnRight)
       (arrangement cySpc CommodityOnRight)

  QSided (Fs side _) nn -> Q (repAnyRadixFromNonNeutral side nn)

  QUnsided n -> Q . QtyRepAnyRadix $ case n of
    NeuCom _ nil -> Left . QtyRep . NilOrBrimPolar
      . Center $ nil
    NeuPer nil -> Right . QtyRep . NilOrBrimPolar
      . Center $ nil

  SCA (Fs sd _) cy -> SC sd . c'Commodity'CommodityA $ cy

  SA sd -> S sd

  UcCyOnLeftA (Fs cy maySpc) nonNeu -> UC
    (c'RepNonNeutralNoSide'NonNeutral nonNeu)
    (c'Commodity'CommodityOnLeftA cy)
    (arrangement maySpc CommodityOnLeft)

  UcCyOnRightA (Fs nonNeu maySpc) cy -> UC
    (c'RepNonNeutralNoSide'NonNeutral nonNeu)
    (c'Commodity'CommodityOnRightA cy)
    (arrangement maySpc CommodityOnRight)

  UA nn -> U (c'RepNonNeutralNoSide'NonNeutral nn)

  CA c -> C (c'Commodity'CommodityA c)

c'Integer'IntegerA :: IntegerA -> Integer
c'Integer'IntegerA (IntegerA ei) = case ei of
  Left Zero -> 0
  Right (mayPluMin, dLeft, dRest)
    -> changeSign
    . (decimalPlace (Prelude.length dRest) dLeft +)
    . foldr f 0
    . zip (iterate pred (Prelude.length dRest - 1))
    $ dRest
    where
      f (ex, num) acc = decimalPlace ex num + acc
      changeSign = case mayPluMin of
        Nothing -> id
        Just Minus -> negate
        Just Plus -> id

c'Scalar'ScalarA :: ScalarA -> Either DateE Scalar
c'Scalar'ScalarA sclr = case sclr of
  ScalarUnquotedString us -> Right . Chars . c'Text'UnquotedString $ us
  ScalarQuotedString qs -> Right . Chars . c'Text'QuotedString $ qs
  ScalarDate dt -> fmap SDate . c'Date'DateA $ dt
  ScalarTime t -> Right . STime . c'Time'TimeA $ t
  ScalarZone (ZoneA _ z) -> Right . SZone $ z
  ScalarInt i -> Right . SInt . c'Integer'IntegerA $ i


arrangement :: Maybe a -> Orient -> Arrangement
arrangement may o = Arrangement o . SpaceBetween . isJust $ may

c'Tree'TreeA :: TreeA -> Either (DateE, [DateE]) Tree
c'Tree'TreeA = undefined
