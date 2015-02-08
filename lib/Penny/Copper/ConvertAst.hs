{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.ConvertAst where

import Control.Arrow (first)
import Penny.Lincoln
import Penny.Copper.Ast
import Penny.Copper.Date
import Penny.Copper.Parser
import Penny.Copper.Terminals
import Data.Text (Text)
import qualified Data.Text as X
import Data.Maybe (mapMaybe, isJust)
import Data.Monoid
import Data.Foldable (foldrM)

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
  = TrioE TrioError (Located PstgMeta)
  | ImbalancedE ImbalancedError PostingList
  | PriceSameCommodityE LineColPosA FromCy
  deriving (Eq, Ord, Show)

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

c'Scalar'ScalarA :: ScalarA -> Scalar
c'Scalar'ScalarA sclr = case sclr of
  ScalarUnquotedString us -> Chars . c'Text'UnquotedString $ us
  ScalarQuotedString qs -> Chars . c'Text'QuotedString $ qs
  ScalarDate dt -> SDate . c'Date'Day . c'Day'DateA $ dt
  ScalarTime t -> STime . c'Time'TimeA $ t
  ScalarZone (ZoneA _ z) -> SZone $ z
  ScalarInt i -> SInt . c'Integer'IntegerA $ i


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

c'Tree'TreeA :: TreeA -> Tree
c'Tree'TreeA (TreeA (Located loc scl) mayBs) =
  Tree User scl' (locationTree loc : rs)
  where
    scl' = c'Scalar'ScalarA scl
    rs = maybe [] (\(Bs _ bf) -> c'ListTree'BracketedForest bf) mayBs

c'ListTree'BracketedForest
  :: BracketedForest
  -> [Tree]
c'ListTree'BracketedForest (BracketedForest _ mayF _) = case mayF of
  Nothing -> []
  Just (Fs (ForestA t1 ts) _) -> map c'Tree'TreeA . (t1 :) . map snd $ ts

c'TopLine'TopLineA :: TopLineA -> TopLine
c'TopLine'TopLineA (TopLineA t1 list)
  = TopLine . map c'Tree'TreeA . (t1 : ) . map snd $ list

c'LocatedPstgMeta'LocatedPostingA
  :: Located PostingA
  -> Located PstgMeta
c'LocatedPstgMeta'LocatedPostingA lctd@(Located lcp _) = fmap f lctd
  where
    f pa = case pa of
      PostingTrioFirst (Located _ trioA) Nothing ->
        PstgMeta [] (c'Trio'TrioA trioA)
      PostingTrioFirst (Located _ trioA) (Just (Bs _ bf)) ->
        PstgMeta (locationTree lcp : c'ListTree'BracketedForest bf)
                 (c'Trio'TrioA trioA)
      PostingNoTrio bf ->
        PstgMeta (locationTree lcp : c'ListTree'BracketedForest bf) E


prependPstgToEnts
  :: Located PstgMeta
  -> Ents PstgMeta
  -> Either ConvertE (Ents PstgMeta)
prependPstgToEnts lctd@(Located _ pm@(PstgMeta _ tri)) ents =
  case prependTrio tri ents of
    Left e -> Left $ TrioE e lctd
    Right g -> Right (g pm)

addLocationToEnts :: LineColPosA -> Ents PstgMeta -> Ents PstgMeta
addLocationToEnts lcp = fmap f
  where
    f (PstgMeta ts tri) = PstgMeta (locationTree lcp : ts) tri

entsFromPostingList
  :: PostingList
  -> Either ConvertE (Balanced PstgMeta)
entsFromPostingList pstgList = do
  let pstgs = case pstgList of
        OnePosting pstg -> [pstg]
        PostingList p1 _ (Bs _ p2) ps ->
          p1 : p2 : map (\(_, Bs _ p) -> p) ps
  ents <- foldrM prependPstgToEnts mempty
    . map c'LocatedPstgMeta'LocatedPostingA
    $ pstgs
  case entsToBalanced ents of
    Left e -> Left (ImbalancedE e pstgList)
    Right g -> return g

c'Balanced'PostingsA :: PostingsA -> Either ConvertE (Balanced PstgMeta)
c'Balanced'PostingsA (PostingsA _ may _) = case may of
  Nothing -> return mempty
  Just (Fs pl _) -> entsFromPostingList pl


c'Transaction'TransactionA
  :: TransactionA
  -> Either ConvertE Transaction
c'Transaction'TransactionA txn = case txn of
  TransactionWithTopLine (Located _ tl) (Bs _ pstgs) -> do
    bal <- c'Balanced'PostingsA pstgs
    return $ Transaction (c'TopLine'TopLineA tl) bal
  TransactionNoTopLine pstgs -> do
    bal <- c'Balanced'PostingsA pstgs
    return $ Transaction (TopLine []) bal

c'Exch'Neutral :: Neutral -> Exch
c'Exch'Neutral neu = case neu of
  NeuCom _ nil -> toExch (ExchRep (NilOrBrimPolar (Center nil)))
  NeuPer nil -> toExch (ExchRep (NilOrBrimPolar (Center nil)))

c'Exch'NonNeutral :: Maybe PluMin -> NonNeutral -> Exch
c'Exch'NonNeutral mp nn = case nn of
  NonNeutralRadCom _ br ->
    toExch (ExchRep (NilOrBrimPolar (OffCenter br pm)))
  NonNeutralRadPer br ->
    toExch (ExchRep (NilOrBrimPolar (OffCenter br pm)))
  where
    pm = case mp of
      Nothing -> Plus
      Just m -> m

c'Exch'ExchA
  :: ExchA
  -> Exch
c'Exch'ExchA exch = case exch of
  ExchANeutral n -> c'Exch'Neutral n
  ExchANonNeutral m n -> c'Exch'NonNeutral (fmap (\(Fs x _) -> x) m) n

c'CommodityExch'CyExch :: CyExch -> (Commodity, Exch)
c'CommodityExch'CyExch cye = (c'Commodity'CommodityA cy, c'Exch'ExchA ea)
  where
    (cy, ea) = case cye of
      CyExchCy (Fs c _) e -> (c, e)
      CyExchA (Fs e _) c -> (c, e)


c'Price'PriceA :: PriceA -> Either ConvertE Price
c'Price'PriceA
  (PriceA lcp _ (Located _ dte) _ mayTime mayZone frA _ cyExch)
  = case fromTo frCy toC of
      Nothing -> Left $ PriceSameCommodityE lcp frCy
      Just frTo -> Right $ Price dt frTo exch
  where
    frCy = FromCy . c'Commodity'CommodityA $ frA
    (toC, exch) = first ToCy . c'CommodityExch'CyExch $ cyExch
    ti = case mayTime of
      Nothing -> midnight
      Just (Located _ (t, _)) -> c'Time'TimeA t
    zn = case mayZone of
      Nothing -> utcZone
      Just (Located _ (ZoneA _ z, _)) -> z
    dt = DateTime (c'Date'Day . c'Day'DateA $ dte) ti zn

c'Either'FileItem
  :: FileItem
  -> Either ConvertE (Either Price Transaction)
c'Either'FileItem (FileItem (Located _ ei)) = case ei of
  Left p -> fmap Left $ c'Price'PriceA p
  Right t -> fmap Right $ c'Transaction'TransactionA t

c'Eithers'FileItems
  :: FileItems
  -> Either (ConvertE, [ConvertE]) [Either Price Transaction]
c'Eithers'FileItems (FileItems i1 is)
  = foldr f (Right []) . (i1:) . map snd $ is
  where
    f item rest = case (c'Either'FileItem item, rest) of
      (Right new, Right old) -> Right (new : old)
      (Right _, Left ers) -> Left ers
      (Left er, Right _) -> Left (er, [])
      (Left er, Left (e1, ers)) -> Left (er, e1 : ers)

convertItemsFromAst
  :: Ast
  -> Either (ConvertE, [ConvertE]) [Either Price Transaction]
convertItemsFromAst f = case f of
  AstNoLeadingWhite (Fs fi _) -> c'Eithers'FileItems fi
  AstLeadingWhite _ Nothing -> Right []
  AstLeadingWhite _ (Just (Fs fi _)) -> c'Eithers'FileItems fi
  EmptyFile -> Right []
