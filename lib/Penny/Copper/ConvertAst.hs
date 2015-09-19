{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Penny.Copper.ConvertAst where

import Control.Lens
import Control.Arrow (first)
import Penny.Arrangement
import Penny.Friendly
import Penny.Copper.Ast
import Penny.Copper.Date
import Penny.Copper.Parser
import Penny.Copper.Terminals
import Penny.Representation
import Penny.DateTime
import Penny.Decimal
import Penny.Display
import Penny.Commodity
import Penny.Ents
import Penny.NonZero
import Penny.Trio
import Penny.Price
import Penny.PluMin
import Penny.Polar
import Penny.Realm
import Penny.Scalar
import Penny.Tree
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import Data.Maybe (mapMaybe, isJust)
import Data.Foldable (foldlM)


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
  | PriceSameCommodityE LineColPosA FromCy
  deriving Show

instance Friendly ConvertE where
  friendly err = ("Error at " ++ display lcp "" ++ ":") : rest
    where
      (lcp, rest) = case err of
        TrioE te (Located l _) -> (l, friendly te)
        ImbalancedE e pl -> (lc, friendly e)
          where
            lc = case pl of
              OnePosting (Located x _) -> x
              PostingList (Located x _) _ _ _ -> x
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

arrangement :: Maybe a -> Orient -> Arrangement
arrangement may o = Arrangement o . isJust $ may

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

  QUnsided n -> Q $ case n of
    NeuCom _ nil -> Left . Moderate $ nil
    NeuPer nil -> Right . Moderate $ nil

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
  ScalarUnquotedString us -> SText . c'Text'UnquotedString $ us
  ScalarQuotedString qs -> SText . c'Text'QuotedString $ qs
  ScalarDate dt -> SDate . c'Date'Day . c'Day'DateA $ dt
  ScalarTime t -> STime . c'Time'TimeA $ t
  ScalarZone (ZoneA _ z) -> SZone $ z
  ScalarInt i -> SInteger . c'Integer'IntegerA $ i


location :: Text
location = "location"

line :: Text
line = "line"

column :: Text
column = "column"

locationTree :: LineColPosA -> Tree
locationTree (LineColPosA lin col pos)
  = sys (Just $ SText "location") . Seq.fromList $
    [ sys (Just $ SText "line")
        (Seq.singleton $ sys (Just . SInteger . fromIntegral $ lin) Seq.empty)
    , sys (Just $ SText "column")
        (Seq.singleton $ sys (Just . SInteger . fromIntegral $ col) Seq.empty)
    , sys (Just $ SText "position")
        (Seq.singleton $ sys (Just . SInteger . fromIntegral $ pos) Seq.empty)
    ]
  where
    sys = Tree System

c'Forest'ForestA :: ForestA -> Seq Tree
c'Forest'ForestA (ForestA t1 ls)
  = c'Tree'TreeA t1 <| (Seq.fromList . map (\(_, Bs _ t) -> c'Tree'TreeA t) $ ls)

c'TopLine'TopLineA :: TopLineA -> Seq Tree
c'TopLine'TopLineA (TopLineA t1) = c'Forest'ForestA t1

c'ListTree'BracketedForest
  :: BracketedForest
  -> Seq Tree
c'ListTree'BracketedForest (BracketedForest _ mayF _) = case mayF of
  Nothing -> Seq.empty
  Just (Fs frst _) -> c'Forest'ForestA frst

c'Tree'TreeA :: TreeA -> Tree

c'Tree'TreeA (TreeScalarFirst (Located loc scl) Nothing)
  = Tree User (Just . c'Scalar'ScalarA $ scl)
              (Seq.singleton $ locationTree loc)

c'Tree'TreeA (TreeScalarFirst (Located loc scl) (Just (Bs _ bf)))
  = Tree User (Just . c'Scalar'ScalarA $ scl)
              (locationTree loc <| c'ListTree'BracketedForest bf)

c'Tree'TreeA (TreeForestFirst bf Nothing)
  = Tree User Nothing (c'ListTree'BracketedForest bf)

c'Tree'TreeA (TreeForestFirst bf (Just (Bs _ (Located loc scl))))
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
      PostingTrioFirst (Located _ trioA) (Just (Bs _ bf)) ->
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
  :: LineColPosA
  -> Ents (Seq Tree, Trio)
  -> Ents (Seq Tree, Trio)
addLocationToEnts lcp = fmap f
  where
    f (ts, tri) = (locationTree lcp <| ts, tri)

entsFromPostingList
  :: PostingList
  -> Either ConvertE (Balanced (Seq Tree))
entsFromPostingList pstgList = do
  let pstgs = case pstgList of
        OnePosting pstg -> Seq.singleton pstg
        PostingList p1 _ (Bs _ p2) ps ->
          p1 <| p2 <| (Seq.fromList $ map (\(_, Bs _ p) -> p) ps)
  ents <- foldlM appendPstgToEnts mempty
    . fmap c'LocatedPstgMeta'LocatedPostingA
    $ pstgs
  case entsToBalanced ents of
    Left e -> Left (ImbalancedE e pstgList)
    Right g -> return g

c'Balanced'PostingsA :: PostingsA -> Either ConvertE (Balanced (Seq Tree))
c'Balanced'PostingsA (PostingsA _ may _) = case may of
  Nothing -> return mempty
  Just (Fs pl _) -> entsFromPostingList pl

c'Transaction'TransactionA
  :: TransactionA
  -> Either ConvertE (Seq Tree, Balanced (Seq Tree))
c'Transaction'TransactionA txn = case txn of
  TransactionWithTopLine (Located _ tl) (Bs _ pstgs) -> do
    bal <- c'Balanced'PostingsA pstgs
    return (c'TopLine'TopLineA tl, bal)
  TransactionNoTopLine pstgs -> do
    bal <- c'Balanced'PostingsA pstgs
    return (Seq.empty, bal)

c'Exch'Neutral :: Neutral -> Decimal
c'Exch'Neutral neu = case neu of
  NeuCom _ nil -> toDecimal . toDecZero $ nil
  NeuPer nil -> toDecimal . toDecZero $ nil

c'Exch'NonNeutral :: Maybe PluMin -> NonNeutral -> Decimal
c'Exch'NonNeutral mp nn = toDecimal . fmap nonZeroToInteger
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

c'Exch'ExchA
  :: ExchA
  -> Decimal
c'Exch'ExchA exch = case exch of
  ExchANeutral n -> c'Exch'Neutral n
  ExchANonNeutral m n -> c'Exch'NonNeutral (fmap (\(Fs x _) -> x) m) n

c'CommodityExch'CyExch :: CyExch -> (Commodity, Decimal)
c'CommodityExch'CyExch cye = (c'Commodity'CommodityA cy, c'Exch'ExchA ea)
  where
    (cy, ea) = case cye of
      CyExchCy (Fs c _) e -> (c, e)
      CyExchA (Fs e _) c -> (c, e)


c'Price'PriceA :: PriceA -> Either ConvertE Price
c'Price'PriceA
  (PriceA lcp _ (Located _ dte) _ mayTime mayZone frA _ cyExch)
  = case makeFromTo frCy toC of
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
  -> Either ConvertE (Either Price (Seq Tree, Balanced (Seq Tree)))
c'Either'FileItem (FileItem (Located _ ei)) = case ei of
  Left p -> fmap Left $ c'Price'PriceA p
  Right t -> fmap Right $ c'Transaction'TransactionA t

c'Eithers'FileItems
  :: FileItems
  -> Either (ConvertE, Seq ConvertE)
            (Seq (Either Price (Seq Tree, Balanced (Seq Tree))))
c'Eithers'FileItems (FileItems i1 is)
  = foldr f (Right Seq.empty) . (i1:) . map snd $ is
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
convertItemsFromAst f = case f of
  AstNoLeadingWhite (Fs fi _) -> c'Eithers'FileItems fi
  AstLeadingWhite _ Nothing -> Right Seq.empty
  AstLeadingWhite _ (Just (Fs fi _)) -> c'Eithers'FileItems fi
  EmptyFile -> Right Seq.empty

