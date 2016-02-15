{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Penny.Copper.ConvertAst where

{-

import Control.Lens
import Control.Arrow (first)
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import Data.Maybe (mapMaybe)
import Data.Foldable (foldlM)
import Text.Megaparsec (SourcePos, sourceLine, sourceColumn)

import Penny.Arrangement
import Penny.Friendly
import Penny.Copper.Ast
import Penny.Copper.Date
import Penny.Copper.Terminals
import Penny.Representation
import Penny.DateTime
import Penny.Decimal
import Penny.Display
import Penny.Commodity
import Penny.Ents
import Penny.Natural
import Penny.NonZero
import Penny.Trio
import Penny.Price
import Penny.PluMin
import Penny.Polar
import Penny.Realm
import Penny.Scalar
import Penny.Tree

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
  = TrioE TrioError (Located (Seq Tree, Trio))
  | ImbalancedE ImbalancedError PostingList
  | PriceSameCommodityE SourcePos FromCy
  deriving Show

instance Friendly ConvertE where
  friendly err = ("Error at " ++ display lcp "" ++ ":") : rest
    where
      (lcp, rest) = case err of
        TrioE te (Located l _) -> (l, friendly te)
        ImbalancedE e pl -> (lc, friendly e)
          where
            PostingList (Located lc _) _ = pl
        PriceSameCommodityE l (FromCy cy) ->
          (l, ["From and To commodities are the same: " ++ X.unpack cy])

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
  = X.pack
  . (termToChar d1 :)
  $ map termToChar ds

c'Commodity'UnquotedCommodityOnRight
  :: UnquotedCommodityOnRight
  -> Commodity
c'Commodity'UnquotedCommodityOnRight (UnquotedCommodityOnRight d1 ds)
  = X.pack
  . (termToChar d1 :)
  $ map termToChar ds

c'Commodity'QuotedCommodity
  :: QuotedCommodity
  -> Commodity
c'Commodity'QuotedCommodity (QuotedCommodity q)
  = c'Text'QuotedString q

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

qtyRepAnyRadix :: NonNeutral -> Pole -> RepAnyRadix
qtyRepAnyRadix nn s = case nn of
  NonNeutralRadCom _ brim -> Left (Extreme (Polarized brim s))
  NonNeutralRadPer brim -> Right (Extreme (Polarized brim s))


c'RepNonNeutralNoSide'NonNeutral :: NonNeutral -> BrimScalarAnyRadix
c'RepNonNeutralNoSide'NonNeutral x = case x of
  NonNeutralRadCom _ b -> Left b
  NonNeutralRadPer p -> Right p

repAnyRadixFromNonNeutral :: Pole -> NonNeutral -> RepAnyRadix
repAnyRadixFromNonNeutral s nn = case nn of
    NonNeutralRadCom _ br -> Left $ Extreme (Polarized br s)
    NonNeutralRadPer br -> Right $ Extreme (Polarized br s)

arrangement :: [a] -> Orient -> Arrangement
arrangement may o = Arrangement o . not . null $ may

c'Trio'TrioA :: TrioA -> Trio
c'Trio'TrioA trio = case trio of
  QcCyOnLeftA (Fs side _) (Fs cy cySpc) (Fs nn _) ->
    QC (qtyRepAnyRadix nn side)
       (c'Commodity'CommodityOnLeftA cy)
       (arrangement cySpc CommodityOnLeft)

  QcCyOnRightA (Fs side _) (Fs nn cySpc) (Fs cyOnRight _) ->
    QC (qtyRepAnyRadix nn side)
       (c'Commodity'CommodityOnRightA cyOnRight)
       (arrangement cySpc CommodityOnRight)

  QSided (Fs side _) (Fs nn _) -> Q (repAnyRadixFromNonNeutral side nn)

  QUnsided (Fs n _) -> Q $ case n of
    NeuCom _ nil -> Left . Moderate $ nil
    NeuPer nil -> Right . Moderate $ nil

  SCA (Fs sd _) (Fs cy _) -> SC sd . c'Commodity'CommodityA $ cy

  SA (Fs sd _) -> S sd

  UcCyOnLeftA (Fs cy maySpc) (Fs nonNeu _) -> UC
    (c'RepNonNeutralNoSide'NonNeutral nonNeu)
    (c'Commodity'CommodityOnLeftA cy)
    (arrangement maySpc CommodityOnLeft)

  UcCyOnRightA (Fs nonNeu maySpc) (Fs cy _) -> UC
    (c'RepNonNeutralNoSide'NonNeutral nonNeu)
    (c'Commodity'CommodityOnRightA cy)
    (arrangement maySpc CommodityOnRight)

  UA (Fs nn _) -> U (c'RepNonNeutralNoSide'NonNeutral nn)

  CA (Fs c _) -> C (c'Commodity'CommodityA c)

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
  ScalarUnquotedString (Fs us _) -> SText . c'Text'UnquotedString $ us
  ScalarQuotedString (Fs qs _) -> SText . c'Text'QuotedString $ qs
  ScalarDate (Fs dt _) -> SDay . dateToDay . c'Date'Day . c'Day'DateA $ dt
  ScalarTime (Fs t _) -> STime . c'TimeOfDay'Time . c'Time'TimeA $ t
  ScalarZone (Fs (ZoneA _ z) _) -> SZone . c'Int'Zone $ z
  ScalarInt (Fs i _) -> SInteger . c'Integer'IntegerA $ i


location :: Text
location = "location"

line :: Text
line = "line"

column :: Text
column = "column"

locationTree :: SourcePos -> Tree
locationTree pos
  = sys (Just $ SText "location") . Seq.fromList $
    [ sys (Just $ SText "line")
        (Seq.singleton $ sys (Just . SInteger . fromIntegral $ lin) Seq.empty)
    , sys (Just $ SText "column")
        (Seq.singleton $ sys (Just . SInteger . fromIntegral $ col) Seq.empty)
    ]
  where
    sys = Tree System
    lin = sourceLine pos
    col = sourceColumn pos

c'Forest'ForestA :: ForestA -> Seq Tree
c'Forest'ForestA (ForestA (Fs t1 _) ls)
  = c'Tree'TreeA t1 <| (Seq.fromList . map (\(_, Fs t _) -> c'Tree'TreeA t) $ ls)

c'TopLine'TopLineA :: TopLineA -> Seq Tree
c'TopLine'TopLineA (TopLineA t1) = c'Forest'ForestA t1

c'ListTree'BracketedForest
  :: BracketedForest
  -> Seq Tree
c'ListTree'BracketedForest (BracketedForest _ mayF _) = case mayF of
  Nothing -> Seq.empty
  Just frst -> c'Forest'ForestA frst

c'Tree'TreeA :: TreeA -> Tree

c'Tree'TreeA (TreeScalarFirst (Located loc scl) Nothing)
  = Tree User (Just . c'Scalar'ScalarA $ scl)
              (Seq.singleton $ locationTree loc)

c'Tree'TreeA (TreeScalarFirst (Located loc scl) (Just bf))
  = Tree User (Just . c'Scalar'ScalarA $ scl)
              (locationTree loc <| c'ListTree'BracketedForest bf)

c'Tree'TreeA (TreeForestFirst bf Nothing)
  = Tree User Nothing (c'ListTree'BracketedForest bf)

c'Tree'TreeA (TreeForestFirst bf (Just (Located loc scl)))
  = Tree User (Just . c'Scalar'ScalarA $ scl)
              (locationTree loc <| c'ListTree'BracketedForest bf)

c'LocatedPstgMeta'LocatedPostingA
  :: Located PostingA
  -> Located (Seq Tree, Trio)
c'LocatedPstgMeta'LocatedPostingA lctd@(Located lcp _) = fmap f lctd
  where
    f pa = case pa of
      PostingTrioFirst (Located _ trioA) Nothing ->
        (Seq.empty, (c'Trio'TrioA trioA))
      PostingTrioFirst (Located _ trioA) (Just bf) ->
        (locationTree lcp <| c'ListTree'BracketedForest bf,
          (c'Trio'TrioA trioA))
      PostingNoTrio bf ->
        ((locationTree lcp <| c'ListTree'BracketedForest bf), E)


appendPstgToEnts
  :: Ents (Seq Tree)
  -> Located (Seq Tree, Trio)
  -> Either ConvertE (Ents (Seq Tree))
appendPstgToEnts ents lctd@(Located _ (ts, tri)) =
  case appendTrio ents tri of
    Left e -> Left $ TrioE e lctd
    Right g -> Right (g ts)

addLocationToEnts
  :: SourcePos
  -> Ents (Seq Tree, Trio)
  -> Ents (Seq Tree, Trio)
addLocationToEnts lcp = fmap f
  where
    f (ts, tri) = (locationTree lcp <| ts, tri)

entsFromPostingList
  :: PostingList
  -> Either ConvertE (Balanced (Seq Tree))
entsFromPostingList pstgList@(PostingList p1 ps) = do
  let pstgs = p1 <| Seq.fromList (map snd ps)
  ents <- foldlM appendPstgToEnts mempty
    . fmap c'LocatedPstgMeta'LocatedPostingA
    $ pstgs
  case entsToBalanced ents of
    Left e -> Left (ImbalancedE e pstgList)
    Right g -> return g

c'Balanced'PostingsA :: PostingsA -> Either ConvertE (Balanced (Seq Tree))
c'Balanced'PostingsA (PostingsA _ may _) = case may of
  Nothing -> return mempty
  Just pl -> entsFromPostingList pl

c'Transaction'TransactionA
  :: TransactionA
  -> Either ConvertE (Seq Tree, Balanced (Seq Tree))
c'Transaction'TransactionA (TransactionA mayTopLine pstgs) = do
  bal <- c'Balanced'PostingsA pstgs
  return $ case mayTopLine of
    Nothing -> (Seq.empty, bal)
    Just tl -> (c'TopLine'TopLineA tl, bal)

c'DecZero'Neutral :: Neutral -> DecZero
c'DecZero'Neutral neu = case neu of
  NeuCom _ nil -> toDecZero nil
  NeuPer nil -> toDecZero nil

c'DecPositive'NonNeutral :: NonNeutral -> DecPositive
c'DecPositive'NonNeutral nn = case nn of
  NonNeutralRadCom _ br -> toDecPositive br
  NonNeutralRadPer br -> toDecPositive br

c'DecUnsigned'NeutralOrNon :: NeutralOrNon -> DecUnsigned
c'DecUnsigned'NeutralOrNon nn = case nn of
  NeutralOrNonRadCom _ ei -> either ((toUnsigned Zero <$) . toDecZero)
    (fmap positiveToUnsigned . toDecPositive) ei
  NeutralOrNonRadPer ei -> either ((toUnsigned Zero <$) . toDecZero)
    (fmap positiveToUnsigned . toDecPositive) ei

c'Decimal'NonNeutral :: Maybe PluMin -> NonNeutral -> Decimal
c'Decimal'NonNeutral mp nn = toDecimal . fmap nonZeroToInteger
  . align pole . fmap c'NonZero'Positive
  $ decPositive
  where
    decPositive = case nn of
      NonNeutralRadCom _ br -> toDecPositive br
      NonNeutralRadPer br -> toDecPositive br
    pole = case mp of
      Nothing -> positive
      Just Plus -> positive
      Just Minus -> negative

c'Decimal'ExchA
  :: ExchA
  -> Decimal
c'Decimal'ExchA exch = case exch of
  ExchANeutral (Fs n _) -> toDecimal . c'DecZero'Neutral $ n
  ExchANonNeutral m (Fs n _) -> c'Decimal'NonNeutral (fmap (\(Fs x _) -> x) m) n

c'CommodityExch'CyExch :: CyExch -> (Commodity, Decimal)
c'CommodityExch'CyExch cye = (c'Commodity'CommodityA cy, c'Decimal'ExchA ea)
  where
    (cy, ea) = case cye of
      CyExchCy (Fs c _) e -> (c, e)
      CyExchA e (Fs c _) -> (c, e)


c'Price'PriceA :: SourcePos -> PriceA -> Either ConvertE Price
c'Price'PriceA lcp
  (PriceA _ dte _ mayTime mayZone frA _ cyExch)
  = case makeFromTo frCy toC of
      Nothing -> Left $ PriceSameCommodityE lcp frCy
      Just frTo -> Right $ Price dt frTo exch
  where
    frCy = FromCy . c'Commodity'CommodityA $ frA
    (toC, exch) = first ToCy . c'CommodityExch'CyExch $ cyExch
    ti = case mayTime of
      Nothing -> midnight
      Just (t, _) -> c'Time'TimeA t
    zn = case mayZone of
      Nothing -> utcZone
      Just (ZoneA _ z, _) -> z
    dt = DateTime (c'Date'Day . c'Day'DateA $ dte) ti zn

c'Either'FileItem
  :: FileItem
  -> Either ConvertE (Either Price (Seq Tree, Balanced (Seq Tree)))
c'Either'FileItem (FileItem (Located lcp ei)) = case ei of
  Left p -> fmap Left $ c'Price'PriceA lcp p
  Right t -> fmap Right $ c'Transaction'TransactionA t

c'Eithers'FileItems
  :: [FileItem]
  -> Either (ConvertE, Seq ConvertE)
            (Seq (Either Price (Seq Tree, Balanced (Seq Tree))))
c'Eithers'FileItems = foldr f (Right Seq.empty)
  where
    f item rest = case (c'Either'FileItem item, rest) of
      (Right new, Right old) -> Right (new <| old)
      (Right _, Left ers) -> Left ers
      (Left er, Right _) -> Left (er, Seq.empty)
      (Left er, Left (e1, ers)) -> Left (er, e1 <| ers)

convertItemsFromAst
  :: Ast
  -> Either (ConvertE, Seq ConvertE)
            (Seq (Either Price (Seq Tree, Balanced (Seq Tree))))
convertItemsFromAst (Ast _ ls) = c'Eithers'FileItems ls
-}
