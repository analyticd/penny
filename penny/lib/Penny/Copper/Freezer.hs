{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}

-- | The Freezer takes Penny types and converts them to Copper types
-- for storage in a file.  Related functions, and more commentary, are
-- in "Penny.Copper.Copperize".
module Penny.Copper.Freezer where

import Penny.Amount (Amount(Amount))
import Penny.Arrangement
import qualified Penny.Commodity as Commodity
import Penny.Copper.Copperize
import Penny.Copper.PriceParts
import Penny.Copper.Quasi
import Penny.Copper.Tracompri
import Penny.Copper.Types
import Penny.Decimal
import Penny.Ents
import qualified Penny.Fields as Fields
import qualified Penny.Tree as Tree
import qualified Penny.Scalar as Scalar
import qualified Penny.NonZero as NZ
import Penny.Polar
import Penny.Rep
import Penny.SeqUtil
import qualified Penny.Tranche as Tranche
import qualified Penny.Transaction as Txn (Transaction(Transaction))
import qualified Penny.Troika as Troika

import qualified Control.Lens as Lens
import Data.Semigroup (Semigroup((<>)))
import Accuerr (Accuerr)
import qualified Accuerr
import qualified Data.OFX as OFX
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (Day, TimeOfDay)
import qualified Data.Time as Time
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Data.Sequence.NonEmpty as NE

-- # Comments

comment
  :: Text
  -> Accuerr (NonEmptySeq Char) (Comment Char ())
comment = cComment

-- # Scalars

text
  :: Text
  -> Either (UnquotedString Char ()) (QuotedString Char ())
text = cString . Seq.fromList . X.unpack

day
  :: Day
  -> Maybe (Date Char ())
day = cDay

time
  :: TimeOfDay
  -> Maybe (Time Char ())
time = cTimeOfDay

integer
  :: Integer
  -> WholeAny Char ()
integer i = case NZ.c'NonZero'Integer i of
  Nothing -> WholeAny'Zero cZero
  Just nz -> WholeAny'WholeNonZero (WholeNonZero pm d1 (D0'9'Star ds))
    where
      pm | p == negative = PluMin'Opt (Just . PluMin'Minus $ cMinus)
         | otherwise = PluMin'Opt Nothing
        where
          p = Lens.view NZ.pole nz
      (d1, ds) = positiveDigits . NZ.c'Positive'NonZero $ nz

-- BROKEN
-- Also check decopperization
zone
  :: Int
  -> Maybe (Zone Char ())
zone i = do
  let (pm, i') | i >= 0 = (PluMin'Plus cPlus, i)
               | otherwise = (PluMin'Minus cMinus, negate i)
  let (hrsInt, minsInt) = i' `divMod` 60
  hours <- c'Hours'Int hrsInt
  minutes <- c'Minutes'Int minsInt
  return . Zone cBacktick $ ZoneHrsMins pm hours minutes

data ScalarError
  = InvalidDay Day
  | InvalidTime TimeOfDay
  | InvalidZone Int
  | InvalidLabel Text
  deriving Show

-- | Creates a single Scalar from a Text.  Never fails.
scalarText :: Text -> Scalar Char ()
scalarText txt = case text txt of
  Left us -> Scalar'UnquotedString us
  Right qs -> Scalar'QuotedString qs

-- | Creates a label Scalar.  Can fail if the Text has characters not
-- valid for a label.
scalarLabel :: Text -> Either ScalarError (Scalar Char ())
scalarLabel txt = case text txt of
  Left us -> Right . Scalar'Label . Label cApostrophe $ us
  Right _ -> Left $ InvalidLabel txt

-- | Creates a Day Scalar.  Can fail if the day is out of the valid
-- range.
scalarDay :: Day -> Either ScalarError (Scalar Char ())
scalarDay dy = case day dy of
  Nothing -> Left $ InvalidDay dy
  Just dt -> Right $ Scalar'Date dt

-- | Creates a Time Scalar.  Can fail if the time is out of the valid
-- range.
scalarTime :: TimeOfDay -> Either ScalarError (Scalar Char ())
scalarTime tod = case time tod of
  Nothing -> Left $ InvalidTime tod
  Just t -> Right $ Scalar'Time t

-- | Creates a Zone Scalar.  Can fail if the zone is out of the valid
-- range.
scalarZone :: Int -> Either ScalarError (Scalar Char ())
scalarZone i = case zone i of
  Nothing -> Left $ InvalidZone i
  Just z -> Right $ Scalar'Zone z

-- | Creates an Integer Scalar.  Never fails.
scalarInteger :: Integer -> Scalar Char ()
scalarInteger = Scalar'WholeAny . integer

scalar
  :: Scalar.Scalar
  -> Either ScalarError (Scalar Char ())
scalar sc = case sc of
  Scalar.SText txt -> Right $ scalarText txt
  Scalar.SLabel txt -> scalarLabel txt
  Scalar.SDay dy -> scalarDay dy
  Scalar.STime tod -> scalarTime tod
  Scalar.SZone i -> scalarZone i
  Scalar.SInteger i -> Right . scalarInteger $ i

-- # Trees


-- | Trees are not frozen if they have no scalar and have no children.
tree
  :: Tree.Tree
  -> Accuerr (NonEmptySeq ScalarError) (Maybe (Tree Char ()))
tree (Tree.Tree s cs) = case s of
  Nothing -> case forest cs of
    Accuerr.AccFailure e -> Accuerr.AccFailure e
    Accuerr.AccSuccess mayForest -> case mayForest of
      Nothing -> Accuerr.AccSuccess Nothing
      Just forest -> Accuerr.AccSuccess . Just . Tree'ForestMaybeScalar
        $ ForestMaybeScalar
        (BracketedForest cOpenSquare mempty forest mempty cCloseSquare)
        (WhitesScalar'Opt Nothing)

  Just sc -> f <$> toAcc (scalar sc) <*> forest cs
    where
      toAcc = either (Accuerr.AccFailure . NE.singleton) Accuerr.AccSuccess
      f scalar mayForest = case mayForest of
        Nothing -> Just . Tree'ScalarMaybeForest
          $ ScalarMaybeForest scalar (WhitesBracketedForest'Opt Nothing)
        Just forest ->
          Just . Tree'ScalarMaybeForest
          $ ScalarMaybeForest scalar
              (WhitesBracketedForest'Opt
                (Just (WhitesBracketedForest mempty
                  (BracketedForest cOpenSquare
                    mempty forest mempty cCloseSquare))))


-- | A forest is not frozen if it is empty.
forest
  :: Seq Tree.Tree
  -> Accuerr (NonEmptySeq ScalarError) (Maybe (Forest Char ()))
forest ts = f <$> traverse tree ts
  where
    f sq = case NE.seqToNonEmptySeq (catMaybes sq) of
      Nothing -> Nothing
      Just (NE.NonEmptySeq t1' ts') -> Just (Forest t1' next)
        where
          next = NextTree'Star (fmap f ts')
            where
              f t = NextTree mempty cComma mempty t

-- # Posting Fields

-- | Creates a tree with a single Text as its scalar and no children.
childlessTextTree :: Text -> Tree Char ()
childlessTextTree x = cTree scalar Nothing
  where
    scalar = case text x of
      Left us -> Scalar'UnquotedString us
      Right qs -> Scalar'QuotedString qs

-- | Creates a tree with a single Integer as its scalar and no children.
childlessIntegerTree :: Integer -> Tree Char ()
childlessIntegerTree i = Tree'ScalarMaybeForest $ ScalarMaybeForest scalar
  (WhitesBracketedForest'Opt Nothing)
  where
    scalar = Scalar'WholeAny . integer $ i

-- | Creates a tree with a single Day as its scalar and no children.
childlessDayTree :: Day -> Either ScalarError (Tree Char ())
childlessDayTree dy = do
  sc <- scalarDay dy
  return $ cTree sc Nothing

-- | Creates a tree with a single TimeOfDay as its scalar and no children.
childlessTimeOfDayTree :: TimeOfDay -> Either ScalarError (Tree Char ())
childlessTimeOfDayTree = fmap (flip cTree Nothing) . scalarTime

-- | Creates a tree with a single Zone as its scalar and no children.
childlessZoneTree :: Int -> Either ScalarError (Tree Char ())
childlessZoneTree = fmap (flip cTree Nothing) . scalarZone

number :: Integer -> Tree Char ()
number = childlessIntegerTree

flag :: Text -> Tree Char ()
flag = childlessTextTree

account :: Seq Text -> Maybe (Tree Char ())
account sq = fmap f $ Lens.uncons sq
  where
    f (a1, as) = orphans (childlessTextTree a1)
      (fmap childlessTextTree as)

-- | Creates a tree with a single parent tree with the given label and
-- with the given 'Text' in a single child.
labeledTextTree
  :: Scalar Char ()
  -- ^ Label for the parent tree; you can get this using 'qLabeled'
  -> Text
  -- ^ Data for the child tree
  -> Tree Char ()
labeledTextTree lbl txt = cTree lbl (Just $ cForest (childlessTextTree txt)
  Seq.empty)

fitid :: Text -> Tree Char ()
fitid = labeledTextTree [qLabeled|fitid|]

tags :: Seq Text -> Maybe (Tree Char ())
tags sq = fmap f $ Lens.uncons sq
  where
    f (a1, as) = cTree [qLabeled|tags|] (Just $ cForest (childlessTextTree a1)
      (fmap childlessTextTree as))

uid :: Text -> Tree Char ()
uid = labeledTextTree [qLabeled|uid|]

trnType :: OFX.TrnType -> Tree Char ()
trnType = labeledTextTree [qLabeled|trnType|] . X.pack . show

origDay :: Day -> Either ScalarError (Tree Char ())
origDay dy = do
  childTree <- childlessDayTree dy
  return $ cTree [qLabeled|origDay|] (Just $ cForest childTree Seq.empty)

origTime :: TimeOfDay -> Either ScalarError (Tree Char ())
origTime ti = do
  childTree <- childlessTimeOfDayTree ti
  return $ cTree [qLabeled|origTime|] (Just $ cForest childTree Seq.empty)

origZone :: Int -> Either ScalarError (Tree Char ())
origZone zn = do
  childTree <- childlessZoneTree zn
  return $ cTree [qLabeled|origZone|] (Just $ cForest childTree Seq.empty)

toAccuerr :: Either e a -> Accuerr (NonEmptySeq e) a
toAccuerr e = case e of
  Left err -> Accuerr.AccFailure (NE.singleton err)
  Right g -> Accuerr.AccSuccess g

-- | Creates a forest for a 'Time.ZonedTime'.  Always includes time of
-- day and zone, even if the time of day is midnight or the zone is
-- UTC.
zonedTime
  :: Time.ZonedTime
  -> Accuerr (NonEmptySeq ScalarError) (Tree Char (), Tree Char (), Tree Char ())
zonedTime (Time.ZonedTime (Time.LocalTime day tod) (Time.TimeZone mins _ _))
  = (,,) <$> dayTree <*> todTree <*> znTree
  where
    dayTree = toAccuerr (childlessDayTree day)
    todTree = toAccuerr (childlessTimeOfDayTree tod)
    znTree = toAccuerr (childlessZoneTree mins)

-- # Top line fields

payee :: Text -> Tree Char ()
payee = childlessTextTree

origPayee :: Text -> Tree Char ()
origPayee txt = cTree [qLabeled|origPayee|]
  (Just $ cForest (childlessTextTree txt) Seq.empty)

topLine
  :: Tranche.TopLine a
  -> Accuerr (NonEmptySeq ScalarError) (TopLine Char ())
topLine (Tranche.Tranche _ trees
  (Fields.TopLineFields fZonedTime fPayee fOrigPayee))
  = f <$> fmap catMaybes (traverse tree trees)
      <*> zonedTime fZonedTime
      <*> pure (fmap payee fPayee)
      <*> pure (fmap origPayee fOrigPayee)
  where
    f ancillaryForest (day, tod, zone) mayPayee mayOrig = TopLine (cForest day rest)
      where
        rest = catMaybes [ Just tod, Just zone, mayPayee, mayOrig ]
          <> ancillaryForest

postingFields
  :: Fields.PostingFields
  -> Accuerr (NonEmptySeq ScalarError) (Seq (Tree Char ()))
postingFields (Fields.PostingFields fNumber fFlag fAccount fFitId
  fTags fUid fTrnType fOrigDay fOrigTime fOrigZone)
  = f <$> mayAcc (fmap origDay fOrigDay)
      <*> mayAcc (fmap origTime fOrigTime)
      <*> mayAcc (fmap origZone fOrigZone)
  where
    mayAcc :: Maybe (Either e a) -> Accuerr (NonEmptySeq e) (Maybe a)
    mayAcc may = case may of
      Nothing -> pure Nothing
      Just ei -> case ei of
        Left e -> Accuerr.AccFailure (NE.singleton e)
        Right g -> Accuerr.AccSuccess (Just g)
    f mayDay mayTime mayZone = catMaybes
      [ fmap number fNumber
      , fmap flag fFlag
      , account fAccount
      , fmap fitid fFitId
      , tags fTags
      , fmap uid fUid
      , fmap trnType fTrnType
      , mayDay
      , mayTime
      , mayZone
      ]

postline
  :: Tranche.Postline a
  -> Accuerr (NonEmptySeq ScalarError) (Maybe (BracketedForest Char ()))
postline (Tranche.Tranche _ trees fields)
  = f <$> postingFields fields <*> traverse tree trees
  where
    f fieldTrees auxTrees
      = fmap g . Lens.uncons $ (fieldTrees <> catMaybes auxTrees)
      where
        g (t1, ts) = cBracketedForest $ cForest t1 ts

-- # Commodity
commodity
  :: Commodity.Commodity
  -> Commodity Char ()
commodity = cCommodity . Seq.fromList . X.unpack


-- # Troiload


troiload
  :: Commodity.Commodity
  -> Troika.Troiload
  -> Maybe (Trio Char ())
troiload cy t = case t of

  Troika.QC (Left (Moderate nilRadCom)) (Arrangement CommodityOnLeft spc)
    -> Just $ Trio'T_Commodity_Neutral (T_Commodity_Neutral (commodity cy)
        (spacer spc) neu)
    where
      neu = NeuCom cBacktick nilRadCom

  Troika.QC (Left (Moderate nilRadCom)) (Arrangement CommodityOnRight spc)
    -> Just $ Trio'T_Neutral_Commodity (T_Neutral_Commodity neu
        (spacer spc) (commodity cy))
    where
      neu = NeuCom cBacktick nilRadCom

  Troika.QC (Left (Extreme (Polarized brimRadCom pole)))
    (Arrangement CommodityOnLeft spc) ->
    Just $ Trio'T_DebitCredit_Commodity_NonNeutral
    (T_DebitCredit_Commodity_NonNeutral (cDebitCredit pole) mempty
      (commodity cy) (spacer spc) brim)
    where
      brim = NonNeutralRadCom cBacktick brimRadCom

  Troika.QC (Left (Extreme (Polarized brimRadCom pole)))
    (Arrangement CommodityOnRight spc) -> Just $
    Trio'T_DebitCredit_NonNeutral_Commodity
    (T_DebitCredit_NonNeutral_Commodity (cDebitCredit pole) mempty
      brim (spacer spc) (commodity cy))
    where
      brim = NonNeutralRadCom cBacktick brimRadCom

  Troika.QC (Right (Moderate nilRadPer)) (Arrangement CommodityOnLeft spc)
    -> Just $ Trio'T_Commodity_Neutral (T_Commodity_Neutral (commodity cy)
        (spacer spc) neu)
    where
      neu = NeuPer nilRadPer

  Troika.QC (Right (Moderate nilRadPer)) (Arrangement CommodityOnRight spc)
    -> Just $ Trio'T_Neutral_Commodity (T_Neutral_Commodity neu
        (spacer spc) (commodity cy))
    where
      neu = NeuPer nilRadPer

  Troika.QC (Right (Extreme (Polarized brimRadPer pole)))
    (Arrangement CommodityOnLeft spc) ->
    Just $ Trio'T_DebitCredit_Commodity_NonNeutral
    (T_DebitCredit_Commodity_NonNeutral (cDebitCredit pole) mempty
      (commodity cy) (spacer spc) brim)
    where
      brim = NonNeutralRadPer brimRadPer

  Troika.QC (Right (Extreme (Polarized brimRadPer pole)))
    (Arrangement CommodityOnRight spc) ->
    Just $ Trio'T_DebitCredit_NonNeutral_Commodity
    (T_DebitCredit_NonNeutral_Commodity (cDebitCredit pole) mempty
      brim (spacer spc) (commodity cy))
    where
      brim = NonNeutralRadPer brimRadPer

  Troika.Q (Left (Moderate nilRadCom)) -> Just $ Trio'T_Neutral (T_Neutral neu)
    where
      neu = NeuCom cBacktick nilRadCom

  Troika.Q (Right (Moderate nilRadPer)) -> Just $ Trio'T_Neutral (T_Neutral neu)
    where
      neu = NeuPer nilRadPer

  Troika.Q (Left (Extreme (Polarized brimRadCom pole))) ->
    Just $ Trio'T_DebitCredit_NonNeutral (T_DebitCredit_NonNeutral
    (cDebitCredit pole) mempty nn)
    where
      nn = NonNeutralRadCom cBacktick brimRadCom

  Troika.Q (Right (Extreme (Polarized brimRadPer pole))) ->
    Just $ Trio'T_DebitCredit_NonNeutral (T_DebitCredit_NonNeutral
    (cDebitCredit pole) mempty nn)
    where
      nn = NonNeutralRadPer brimRadPer

  Troika.SC dnz -> Just $ Trio'T_DebitCredit_Commodity
    (T_DebitCredit_Commodity (cDebitCredit pole) mempty (commodity cy))
    where
      pole = Lens.view poleDecNonZero dnz

  Troika.S dnz -> Just $ Trio'T_DebitCredit
    . T_DebitCredit . cDebitCredit .  Lens.view poleDecNonZero $ dnz

  Troika.UC brim _ (Arrangement CommodityOnLeft spc) ->
    Just $ Trio'T_Commodity_NonNeutral (T_Commodity_NonNeutral (commodity cy)
    (spacer spc) nn)
    where
      nn = case brim of
        Left brc -> NonNeutralRadCom cBacktick brc
        Right brp -> NonNeutralRadPer brp

  Troika.UC brim _ (Arrangement CommodityOnRight spc) ->
    Just $ Trio'T_NonNeutral_Commodity (T_NonNeutral_Commodity nn
    (spacer spc) (commodity cy))
    where
      nn = case brim of
        Left brc -> NonNeutralRadCom cBacktick brc
        Right brp -> NonNeutralRadPer brp

  Troika.NC nil (Arrangement CommodityOnLeft spc) ->
    Just $ Trio'T_Commodity_Neutral (T_Commodity_Neutral (commodity cy)
    (spacer spc) nar)
    where
      nar = case nil of
        Left nrc -> NeuCom cBacktick nrc
        Right nrp -> NeuPer nrp

  Troika.NC nil (Arrangement CommodityOnRight spc) ->
    Just $ Trio'T_Neutral_Commodity (T_Neutral_Commodity nar
    (spacer spc) (commodity cy))
    where
      nar = case nil of
        Left nrc -> NeuCom cBacktick nrc
        Right nrp -> NeuPer nrp

  Troika.US brim _ -> Just $ Trio'T_NonNeutral (T_NonNeutral b)
    where
      b = case brim of
        Left brc -> NonNeutralRadCom cBacktick brc
        Right brp -> NonNeutralRadPer brp

  Troika.UU nar -> Just $ Trio'T_Neutral (T_Neutral n)
    where
      n = case nar of
        Left nrc -> NeuCom cBacktick nrc
        Right nrp -> NeuPer nrp

  Troika.C _ -> Just $ Trio'T_Commodity (T_Commodity (commodity cy))

  Troika.E _ -> Nothing


  where
    spacer useSpace
      | useSpace = space
      | otherwise = mempty

-- # Amounts

-- | Amounts are always frozen as an ungrouped representation with
-- the commodity on the left with no space between.
amount
  :: Amount
  -> Trio Char ()
amount (Amount cy q) = case splitRepAnyRadix rep of
  Left nil -> Trio'T_Commodity_Neutral tComNeu
    where
      tComNeu = T_Commodity_Neutral cy' mempty neu
      neu = case nil of
        Left nilCom -> NeuCom cBacktick nilCom
        Right nilPer -> NeuPer nilPer
  Right (brim, pole) -> Trio'T_DebitCredit_Commodity_NonNeutral tDrComNon
    where
      tDrComNon = T_DebitCredit_Commodity_NonNeutral drCr mempty cy'
        mempty nn
        where
          drCr | pole == debit = DebitCredit'Debit (Debit cLessThan)
               | otherwise = DebitCredit'Credit (Credit cGreaterThan)
          nn = case brim of
            Left brimCom -> NonNeutralRadCom cBacktick brimCom
            Right brimPer -> NonNeutralRadPer brimPer
  where
    rep = repDecimal (Right Nothing) q
    cy' = cCommodity . Seq.fromList . X.unpack $ cy

-- | Succeeds if the 'Troika.Troiquant' is anything other than
-- 'Troika.E'; otherwise, returns Nothing.
troika
  :: Troika.Troika
  -> Maybe (Trio Char ())
troika (Troika.Troika cy tl) = troiload cy tl

-- # Prices
data PriceError
  = BadPriceDay Time.Day
  | BadPriceZone Time.TimeZone
  | BadPriceTime Time.TimeOfDay
  deriving Show

price
  :: PriceParts a
  -> Either PriceError (Price Char ())
price pp = do
  let (Time.ZonedTime (Time.LocalTime dy tod) tz) = _priceTime pp
  dt <- maybe (Left (BadPriceDay dy)) Right . day $ dy
  zon <- maybe (Left (BadPriceZone tz)) Right . zone . Time.timeZoneMinutes $ tz
  tim <- maybe (Left (BadPriceTime tod)) Right . time $ tod
  let from = cCommodity . Seq.fromList . X.unpack . _priceFrom $ pp
      to = cCommodity . Seq.fromList . X.unpack . _priceTo $ pp
      exch = case splitRepAnyRadix . repDecimal (Right Nothing)
                    . _priceExch $ pp of
                Left nil -> Exch'Neutral $ case nil of
                  Left nilCom -> NeuCom cBacktick nilCom
                  Right nilPer -> NeuPer nilPer
                Right (brim, pole) -> Exch'ExchNonNeu $ ExchNonNeu'PluMinNonNeutral
                  (PluMinNonNeutral pm mempty nonNeu)
                  where
                    pm | pole == negative = PluMin'Minus cMinus
                       | otherwise = PluMin'Plus cPlus
                    nonNeu = case brim of
                      Left brimCom -> NonNeutralRadCom cBacktick brimCom
                      Right brimPer -> NonNeutralRadPer brimPer
      jan = Janus'CyExch $ CyExch to mempty exch
      ti = WhitesTime'Opt (Just (WhitesTime mempty tim))
      zn = WhitesZone'Opt (Just (WhitesZone mempty zon))
  return $ Price cAtSign mempty dt ti zn mempty from mempty jan

-- # Transactions

-- | An error occurred when trying to freeze a single price,
-- transaction, or comment.
data TracompriError a
  = TracompriBadForest (Txn.Transaction a) (NonEmptySeq ScalarError)
  -- ^ Could not freeze one or more trees due to a 'ScalarError'.  As
  -- many 'ScalarErrors' as possible are accumulated.  The
  -- 'Transaction' is the entire transaction that we were trying
  -- to freeze.
  | TracompriEmptyPosting (Txn.Transaction a)
  -- ^ When freezing a posting, there must be either a 'Troiload' or a
  -- forest, as there is no way in the grammar to represent a posting
  -- that has neither a 'Troiload' nor a forest.  If there is a
  -- posting with neither, this error is returned.
  | TracompriBadComment Text (NonEmptySeq Char)
  -- ^ Could not freeze a comment line due to at least one invalid
  -- character.  As many invalid characters as possible are
  -- accumulated.  The 'Text' is the entire invalid comment.
  | TracompriBadPrice (PriceParts a) PriceError
  -- ^ Could not freeze a price.  The entire 'PriceParts' is here,
  -- along with the error.
  deriving Show

tracompriComment
  :: Text
  -> Accuerr (NonEmptySeq (TracompriError a)) (Comment Char ())
tracompriComment txt
  = Lens.over Accuerr._AccFailure (NE.singleton . TracompriBadComment txt)
  . comment
  $ txt

tracompriPrice
  :: PriceParts a
  -> Accuerr (NonEmptySeq (TracompriError a)) (Price Char ())
tracompriPrice p
  = Lens.over Accuerr._AccFailure (NE.singleton . TracompriBadPrice p)
  . Lens.view Accuerr.isoEitherAccuerr
  . price
  $ p

tracompriTopLine
  :: Txn.Transaction a
  -> Accuerr (NonEmptySeq (TracompriError a)) (TopLine Char ())
tracompriTopLine parts@(Txn.Transaction tl _) = case topLine tl of
  Accuerr.AccFailure errs -> Accuerr.AccFailure . NE.singleton
    $ TracompriBadForest parts errs
  Accuerr.AccSuccess g -> pure g

combineTracompris
  :: Seq (FileItem Char ())
  -> WholeFile Char ()
combineTracompris fis = WholeFile (WhitesFileItem'Star
  (fmap (WhitesFileItem mempty) fis)) mempty

tracompriPosting
  :: Txn.Transaction a
  -- ^ The entire transaction.  Used only for error messages.
  -> (Troika.Troika, Tranche.Postline a)
  -- ^ This posting
  -> Accuerr (NonEmptySeq (TracompriError a)) (Posting Char ())
tracompriPosting txn (tk, pl) = case postline pl of
  Accuerr.AccFailure errs -> Accuerr.AccFailure . NE.singleton
    $ TracompriBadForest txn errs
  Accuerr.AccSuccess mayForest -> case troika tk of
    Nothing -> case mayForest of
      Nothing -> Accuerr.AccFailure . NE.singleton $ TracompriEmptyPosting txn
      Just bf -> Accuerr.AccSuccess . Posting'BracketedForest $ bf
    Just tri -> Accuerr.AccSuccess
      $ Posting'TrioMaybeForest $ TrioMaybeForest tri mayWhitesBf
      where
        mayWhitesBf = WhitesBracketedForest'Opt $ case mayForest of
          Nothing -> Nothing
          Just for -> Just (WhitesBracketedForest mempty for)

tracompriPostings
  :: Txn.Transaction a
  -> Accuerr (NonEmptySeq (TracompriError a)) (Seq (Posting Char ()))
tracompriPostings parts@(Txn.Transaction _ pstgs)
  = traverse (tracompriPosting parts) . balancedToSeqEnt $ pstgs

tracompriTransaction
  :: Txn.Transaction a
  -> Accuerr (NonEmptySeq (TracompriError a)) (Transaction Char ())
tracompriTransaction parts
  = f
  <$> tracompriTopLine parts
  <*> tracompriPostings parts
  where
    f tl pstgs = case Lens.uncons pstgs of
      Nothing -> Transaction'TopLineMaybePostings
        (TopLineMaybePostings tl (WhitesPostings'Opt Nothing))
      Just (p1, ps) -> Transaction'TopLineMaybePostings
        (TopLineMaybePostings tl
          (WhitesPostings'Opt (Just (WhitesPostings mempty
          (copperPstgs p1 ps)))))
      where
        copperPstgs p1 ps = Postings cOpenCurly (PostingList'Opt
          (Just (PostingList mempty p1 (NextPosting'Star (fmap mkNext ps)))))
          mempty cCloseCurly
          where
            mkNext p = NextPosting mempty cSemicolon mempty p

tracompri
  :: Tracompri a
  -> Accuerr (NonEmptySeq (TracompriError a)) (FileItem Char ())
tracompri x = case x of
  Tracompri'Transaction t -> fmap f (tracompriTransaction t)
    where
      f res = FileItem'Transaction $ res
  Tracompri'Comment txt -> fmap f (tracompriComment txt)
    where
      f com = FileItem'Comment $ com
  Tracompri'Price pp -> fmap f (tracompriPrice . priceToPriceParts $ pp)
    where
      f pri = FileItem'Price $ pri

wholeFile
  :: Seq (Tracompri a)
  -> Accuerr (NonEmptySeq (TracompriError a)) (WholeFile Char ())
wholeFile = fmap combineTracompris . traverse tracompri
