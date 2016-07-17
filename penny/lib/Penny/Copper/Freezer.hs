{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
module Penny.Copper.Freezer where

import Penny.Amount
import Penny.Copper.Copperize
import Penny.Copper.PriceParts
import Penny.Copper.Quasi
import Penny.Copper.Types
import qualified Penny.Fields as Fields
import qualified Penny.Tree as Tree
import qualified Penny.Scalar as Scalar
import qualified Penny.NonZero as NZ
import Penny.Polar
import Penny.Rep
import Penny.SeqUtil
import qualified Penny.Tranche as Tranche

import qualified Control.Lens as Lens
import Control.Monad ((>=>))
import Data.Semigroup (Semigroup((<>)))
import Accuerr (Accuerr)
import qualified Accuerr
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (Day, TimeOfDay)
import qualified Data.Time as Time
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pinchot (NonEmpty)
import qualified Pinchot

-- # Comments

comment
  :: Text
  -> Accuerr (NonEmpty Char) (Comment Char ())
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

zone
  :: Int
  -> Maybe (Zone Char ())
zone i = do
  let (pm, i') | i >= 0 = (PluMin'Plus cPlus, i)
               | otherwise = (PluMin'Minus cMinus, negate i)
  (r, _) <- zoneDigit3 >=> zoneDigit2 >=> zoneDigit1 >=> zoneDigit0
    $ (\d3 d2 d1 d0 -> Zone cBacktick $ ZoneHrsMins pm d3 d2 d1 d0, i')
  return r
  where
    zoneDigit mk divisor (f, i) =
      let (d, m) = i `divMod` divisor
      in case mk d of
          Nothing -> Nothing
          Just dig -> Just (f dig, m)
    zoneDigit3 = zoneDigit c'D0'2'Int 1000
    zoneDigit2 = zoneDigit c'D0'3'Int 100
    zoneDigit1 = zoneDigit c'D0'9'Int 10
    zoneDigit0 = zoneDigit c'D0'9'Int 1

data ScalarError
  = InvalidDay Day
  | InvalidTime TimeOfDay
  | InvalidZone Int
  | InvalidLabel Text

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
  -> Accuerr (NonEmpty ScalarError) (Maybe (Tree Char ()))
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
      toAcc = either (Accuerr.AccFailure . Pinchot.singleton) Accuerr.AccSuccess
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
  -> Accuerr (NonEmpty ScalarError) (Maybe (Forest Char ()))
forest ts = f <$> traverse tree ts
  where
    f sq = case Pinchot.seqToNonEmpty (catMaybes sq) of
      Nothing -> Nothing
      Just (Pinchot.NonEmpty t1' ts') -> Just (Forest t1' next)
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

fitid :: Text -> Tree Char ()
fitid txt = cTree [qLabeled|fitid|] (Just $ cForest (childlessTextTree txt)
  Seq.empty)

tags :: Seq Text -> Maybe (Tree Char ())
tags sq = fmap f $ Lens.uncons sq
  where
    f (a1, as) = cTree [qLabeled|tags|] (Just $ cForest (childlessTextTree a1)
      (fmap childlessTextTree as))

uid :: Text -> Tree Char ()
uid txt = cTree [qLabeled|uid|] (Just $ cForest (childlessTextTree txt)
  Seq.empty)

toAccuerr :: Either e a -> Accuerr (NonEmpty e) a
toAccuerr e = case e of
  Left err -> Accuerr.AccFailure (Pinchot.singleton err)
  Right g -> Accuerr.AccSuccess g

-- | Creates a forest for a 'Time.ZonedTime'.  Always includes time of
-- day and zone, even if the time of day is midnight or the zone is
-- UTC.
zonedTime
  :: Time.ZonedTime
  -> Accuerr (NonEmpty ScalarError) (Tree Char (), Tree Char (), Tree Char ())
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
  -> Accuerr (NonEmpty ScalarError) (TopLine Char ())
topLine (Tranche.Tranche _ trees fields)
  = f <$> fmap catMaybes (traverse tree trees)
      <*> zonedTime (Fields._zonedTime fields)
      <*> pure (fmap payee . Fields._payee $ fields)
      <*> pure (fmap origPayee . Fields._origPayee $ fields)
  where
    f ancillaryForest (day, tod, zone) mayPayee mayOrig = TopLine (cForest day rest)
      where
        rest = catMaybes [ Just tod, Just zone, mayPayee, mayOrig ]
          <> ancillaryForest

postline
  :: Tranche.Postline a
  -> Accuerr (NonEmpty ScalarError) (Maybe (BracketedForest Char ()))
postline (Tranche.Tranche _ trees fields) = fmap f $ traverse tree trees
  where
    f auxTrees
      = fmap g . Lens.uncons . catMaybes $ fieldTrees <> auxTrees
      where
        g (t1, ts) = cBracketedForest $ cForest t1 ts
    fieldTrees =
      [ fmap number . Fields._number $ fields
      , fmap flag . Fields._flag $ fields
      , account . Fields._account $ fields
      , fmap fitid . Fields._fitid $ fields
      , tags . Fields._tags $ fields
      , fmap uid . Fields._uid $ fields
      ]


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

-- # Prices
data PriceError
  = BadPriceDay Time.Day
  | BadPriceZone Time.TimeZone
  | BadPriceTime Time.TimeOfDay

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

data TxnParts = TxnParts
  { _topLine :: Tranche.TopLine ()
  , _postings :: Seq (Tranche.Postline (), Amount)
  }

data Tracompri
  = Tracompri'Transaction TxnParts
  | Tracompri'Comment Text
  | Tracompri'Price (PriceParts ())

data TracompriError
  = TracompriBadForest TxnParts (NonEmpty ScalarError)
  | TracompriBadComment Text (NonEmpty Char)
  | TracompriBadPrice (PriceParts ()) PriceError

tracompriComment
  :: Text
  -> Accuerr (NonEmpty TracompriError) (Comment Char ())
tracompriComment txt
  = Lens.over Accuerr._AccFailure (Pinchot.singleton . TracompriBadComment txt)
  . comment
  $ txt

tracompriPrice
  :: PriceParts ()
  -> Accuerr (NonEmpty TracompriError) (Price Char ())
tracompriPrice p
  = Lens.over Accuerr._AccFailure (Pinchot.singleton . TracompriBadPrice p)
  . Lens.view Accuerr.isoEitherAccuerr
  . price
  $ p

tracompriTopLine
  :: TxnParts
  -> Accuerr (NonEmpty TracompriError) (TopLine Char ())
tracompriTopLine parts@(TxnParts tl _) = case topLine tl of
  Accuerr.AccFailure errs -> Accuerr.AccFailure . Pinchot.singleton
    $ TracompriBadForest parts errs
  Accuerr.AccSuccess g -> pure g

combineTracompris
  :: Seq (FileItem Char ())
  -> WholeFile Char ()
combineTracompris fis = WholeFile (WhitesFileItem'Star
  (fmap (WhitesFileItem mempty) fis)) mempty

tracompriPosting
  :: TxnParts
  -- ^ The entire transaction.  Used only for error messages.
  -> (Tranche.Postline a, Amount)
  -- ^ This posting
  -> Accuerr (NonEmpty TracompriError) (Posting Char ())
tracompriPosting parts (pl, amt) = case postline pl of
  Accuerr.AccFailure errs -> Accuerr.AccFailure . Pinchot.singleton
    $ TracompriBadForest parts errs
  Accuerr.AccSuccess mayForest -> Accuerr.AccSuccess
    $ Posting'TrioMaybeForest $ TrioMaybeForest tri mayWhitesBf
    where
      tri = amount amt
      mayWhitesBf = WhitesBracketedForest'Opt $ case mayForest of
        Nothing -> Nothing
        Just for -> Just (WhitesBracketedForest mempty for)

tracompriPostings
  :: TxnParts
  -> Accuerr (NonEmpty TracompriError) (Seq (Posting Char ()))
tracompriPostings parts@(TxnParts _ pstgs)
  = traverse (tracompriPosting parts) pstgs

tracompriTransaction
  :: TxnParts
  -> Accuerr (NonEmpty TracompriError) (Transaction Char ())
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
  :: Tracompri
  -> Accuerr (NonEmpty TracompriError) (FileItem Char ())
tracompri x = case x of
  Tracompri'Transaction t -> fmap f (tracompriTransaction t)
    where
      f res = FileItem'Transaction $ res
  Tracompri'Comment txt -> fmap f (tracompriComment txt)
    where
      f com = FileItem'Comment $ com
  Tracompri'Price pp -> fmap f (tracompriPrice pp)
    where
      f pri = FileItem'Price $ pri

wholeFile
  :: Seq Tracompri
  -> Accuerr (NonEmpty TracompriError) (WholeFile Char ())
wholeFile = fmap combineTracompris . traverse tracompri

