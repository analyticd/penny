{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.ConvertAst where

import Penny.Lincoln
import Penny.Copper.Ast
import Penny.Copper.Parser
import Penny.Copper.Terminals
import Data.Text (Text)
import qualified Data.Text as X
import Data.Maybe (mapMaybe, isJust)
import Data.Monoid

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

data ConvertE
  = DateE DateA
  -- ^ Bad Date
  | TrioE TrioError PostingA
  | ImbalancedE ImbalancedError PostingList
  deriving (Eq, Ord, Show)

c'Date'DateA :: DateA -> Either ConvertE Date
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

c'Scalar'ScalarA :: ScalarA -> Either ConvertE Scalar
c'Scalar'ScalarA sclr = case sclr of
  ScalarUnquotedString us -> Right . Chars . c'Text'UnquotedString $ us
  ScalarQuotedString qs -> Right . Chars . c'Text'QuotedString $ qs
  ScalarDate dt -> fmap SDate . c'Date'DateA $ dt
  ScalarTime t -> Right . STime . c'Time'TimeA $ t
  ScalarZone (ZoneA _ z) -> Right . SZone $ z
  ScalarInt i -> Right . SInt . c'Integer'IntegerA $ i


arrangement :: Maybe a -> Orient -> Arrangement
arrangement may o = Arrangement o . SpaceBetween . isJust $ may

location :: Text
location = "location"

line :: Text
line = "line"

column :: Text
column = "column"

locationTree :: LineColPosA -> Tree
locationTree (LineColPosA lin col pos)
  = sys (Chars "location")
    [ sys (Chars "line")
        [sys (SInt . fromIntegral $ lin) []]
    , sys (Chars "column")
        [sys (SInt . fromIntegral $ col) []]
    , sys (Chars "position")
        [sys (SInt . fromIntegral $ pos) []]
    ]
  where
    sys = Tree System

c'Tree'TreeA :: TreeA -> Either (ConvertE, [ConvertE]) Tree
c'Tree'TreeA (TreeA (Located loc scl) mayBs)
  = case (scl', bkt) of
      (Left l, Left (r1, rs)) -> Left (l, r1:rs)
      (Left l, Right _) -> Left (l, [])
      (Right _, Left (r1, rs)) -> Left (r1, rs)
      (Right sc, Right rs) ->
        Right $ Tree User sc (locationTree loc : rs)
  where
    scl' = c'Scalar'ScalarA scl
    bkt = maybe (Right [])
      (\(Bs _ bf) -> c'ListTree'BracketedForest bf) mayBs

c'ListTree'BracketedForest
  :: BracketedForest
  -> Either (ConvertE, [ConvertE]) [Tree]
c'ListTree'BracketedForest (BracketedForest _ mayF _) = case mayF of
  Nothing -> Right []
  Just (Fs (ForestA t1 ts) _) ->
    foldr f (Right []) (t1: map snd ts)
    where
      f t ei = case c'Tree'TreeA t of
        Left (l1, ls) -> case ei of
          Left (r1, rs) -> Left (l1, ls ++ r1 : rs)
          Right _ -> Left (l1, ls)
        Right l -> case ei of
          Left (r1, rs) -> Left (r1, rs)
          Right r -> Right $ l : r

c'TopLine'TopLineA :: TopLineA -> Either (ConvertE, [ConvertE]) TopLine
c'TopLine'TopLineA (TopLineA t1 list)
  = fmap TopLine . foldr f (Right []) . (t1:) . map snd $ list
  where
    f t rest = case (c'Tree'TreeA t, rest) of
      ((Left (l1, ls)), (Left (r1, rs))) -> Left (l1, ls ++ r1 : rs)
      (Left l, Right _) -> Left l
      (Right _, Left r) -> Left r
      (Right l, Right r) -> Right (l : r)

pstgMetaFromPostingA
  :: Located PostingA
  -> Either (ConvertE, [ConvertE]) PstgMeta
pstgMetaFromPostingA (Located lcp pa) = case pa of
  PostingTrioFirst (Located _ trioA) Nothing ->
    Right (PstgMeta [] (c'Trio'TrioA trioA))
  PostingTrioFirst (Located _ trioA) (Just (Bs _ bf)) ->
    case c'ListTree'BracketedForest bf of
      Left e -> Left e
      Right g -> Right $ PstgMeta (locationTree lcp : g) (c'Trio'TrioA trioA)
  PostingNoTrio bf -> fmap f (c'ListTree'BracketedForest bf)
    where
      f ts = PstgMeta (locationTree lcp : ts) E


entsFromPostingA
  :: Located PostingA
  -> Ents PstgMeta
  -> Either (ConvertE, [ConvertE]) (Ents PstgMeta)
entsFromPostingA (Located lcp pa) ents =
  case (prependTrio tri ents, eiForest) of
    (Left triE, Left (e1, es)) -> Left (TrioE triE pa, e1:es)
    (Left triE, Right _) -> Left (TrioE triE pa, [])
    (Right _, Left e) -> Left e
    (Right mkEnts, Right ts) -> Right
      (mkEnts (PstgMeta (locationTree lcp : ts) tri))
    where
      (eiForest, tri) = case pa of
        PostingTrioFirst (Located _ trioA) Nothing ->
          (Right [], c'Trio'TrioA trioA)
        PostingTrioFirst (Located _ trioA) (Just (Bs _ bf))
          -> (c'ListTree'BracketedForest bf, c'Trio'TrioA trioA)
        PostingNoTrio bf -> (c'ListTree'BracketedForest bf, E)

addLocationToEnts :: LineColPosA -> Ents PstgMeta -> Ents PstgMeta
addLocationToEnts lcp = fmap f
  where
    f (PstgMeta ts tri) = PstgMeta (locationTree lcp : ts) tri

entsFromPostingList
  :: PostingList
  -> Either (ConvertE, [ConvertE]) (Balanced PstgMeta)
entsFromPostingList pstgList = case pstgList of
  OnePosting pstg -> case entsFromPostingA pstg mempty of
    Left e -> Left e
    Right ents -> case entsToBalanced ents of
      Left e -> Left (ImbalancedE e pstgList, [])
      Right bal -> return bal

  PostingList p1 _ ps1 pss -> finish . foldr f (Right mempty)
    $ (p1 : ps1' : pss')
    where
      getP (Bs _ p) = p
      ps1' = getP ps1
      pss' = map (getP . snd) pss
      finish ei = case ei of
        Left e -> Left e
        Right g -> case entsToBalanced g of
          Left e -> Left (ImbalancedE e pstgList, [])
          Right bal -> Right bal
      f = undefined

