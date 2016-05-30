module Penny.Copper.Freezer where

import Penny.Amount
import Penny.Copper.Copperize
import Penny.Copper.PriceParts
import Penny.Copper.Types
import qualified Penny.Tree as Tree
import qualified Penny.Scalar as Scalar
import qualified Penny.NonZero as NZ
import Penny.Polar
import qualified Penny.Positive as Pos
import Penny.Realm
import Penny.Rep
import Penny.SeqUtil

import qualified Control.Lens as Lens
import Control.Monad ((>=>), join)
import Data.Semigroup (Semigroup((<>)))
import Data.Validation (AccValidation(AccFailure, AccSuccess), _Failure,
  _AccValidation)
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
  -> AccValidation (NonEmpty Char) (Comment Char ())
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

scalar
  :: Scalar.Scalar
  -> Either ScalarError (Scalar Char ())
scalar sc = case sc of
  Scalar.SText txt -> Right $ case text txt of
    Left us -> Scalar'UnquotedString us
    Right qs -> Scalar'QuotedString qs
  Scalar.SLabel txt -> case text txt of
    Left us -> Right . Scalar'Label . Label cApostrophe $ us
    Right qs -> Left $ InvalidLabel txt
  Scalar.SDay dy -> case day dy of
    Nothing -> Left $ InvalidDay dy
    Just dt -> Right $ Scalar'Date dt
  Scalar.STime tod -> case time tod of
    Nothing -> Left $ InvalidTime tod
    Just t -> Right $ Scalar'Time t
  Scalar.SZone i -> case zone i of
    Nothing -> Left $ InvalidZone i
    Just z -> Right $ Scalar'Zone z
  Scalar.SInteger i -> Right . Scalar'WholeAny . integer $ i

-- # Trees


-- | Trees are not frozen if they are in the System realm.
-- Also, they are not frozen if they have no scalar and have no
-- children.
tree
  :: Tree.Tree
  -> AccValidation (NonEmpty ScalarError) (Maybe (Tree Char ()))
tree (Tree.Tree s cs) = case s of
  Nothing -> case forest cs of
    AccFailure e -> AccFailure e
    AccSuccess mayForest -> case mayForest of
      Nothing -> AccSuccess Nothing
      Just forest -> AccSuccess . Just . Tree'ForestMaybeScalar
        $ ForestMaybeScalar
        (BracketedForest cOpenSquare mempty forest mempty cCloseSquare)
        (WhitesScalar'Opt Nothing)

  Just sc -> f <$> toAcc (scalar sc) <*> forest cs
    where
      toAcc = either (AccFailure . Pinchot.singleton) AccSuccess
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


forest
  :: Seq Tree.Tree
  -> AccValidation (NonEmpty ScalarError) (Maybe (Forest Char ()))
forest ts = f <$> traverse tree ts
  where
    f sq = case Pinchot.seqToNonEmpty (catMaybes sq) of
      Nothing -> Nothing
      Just (Pinchot.NonEmpty t1' ts') -> Just (Forest t1' next)
        where
          next = NextTree'Star (fmap f ts')
            where
              f t = NextTree mempty cComma mempty t

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
  { _topLine :: Seq Tree.Tree
  , _postings :: Seq (Seq Tree.Tree, Amount)
  }

data Tracompri
  = Tracompri'Transaction TxnParts
  | Tracompri'Comment Text
  | Tracompri'Price (PriceParts ())

data TracompriError
  = TracompriBadForest TxnParts (NonEmpty ScalarError)
  | TracompriBadComment Text (NonEmpty Char)
  | TracompriBadPrice (PriceParts ()) PriceError

tracompri
  :: Tracompri
  -> AccValidation (NonEmpty TracompriError)
                   (Maybe (FileItem Char ()))
tracompri x = case x of
  Tracompri'Transaction t -> fmap f (tracompriTransaction t)
    where
      f mayRes = case mayRes of
        Nothing -> Nothing
        Just res -> Just . FileItem'Transaction $ res
  Tracompri'Comment txt -> fmap f (tracompriComment txt)
    where
      f com = Just . FileItem'Comment $ com
  Tracompri'Price pp -> fmap f (tracompriPrice pp)
    where
      f pri = Just . FileItem'Price $ pri

tracompriPrice
  :: PriceParts ()
  -> AccValidation (NonEmpty TracompriError) (Price Char ())
tracompriPrice p
  = Lens.over _Failure (Pinchot.singleton . TracompriBadPrice p)
  . Lens.view _AccValidation
  . price
  $ p

tracompriComment
  :: Text
  -> AccValidation (NonEmpty TracompriError) (Comment Char ())
tracompriComment txt
  = Lens.over _Failure (Pinchot.singleton . TracompriBadComment txt)
  . comment
  $ txt

tracompriTransaction
  :: TxnParts
  -> AccValidation (NonEmpty TracompriError) (Maybe (Transaction Char ()))
tracompriTransaction parts
  = f
  <$> tracompriTopLine parts
  <*> tracompriPostings parts
  where
    f mayTl pstgs = case (mayTl, Lens.uncons pstgs) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just (p1, ps)) -> Just (Transaction'Postings
        (copperPstgs p1 ps))
      (Just tl, Just (p1, ps)) -> Just (Transaction'TopLineMaybePostings
        (TopLineMaybePostings tl
          (WhitesPostings'Opt (Just (WhitesPostings mempty
          (copperPstgs p1 ps))))))
      (Just tl, Nothing) -> Just (Transaction'TopLineMaybePostings
        (TopLineMaybePostings tl (WhitesPostings'Opt Nothing)))
      where
        copperPstgs p1 ps = Postings cOpenCurly (PostingList'Opt
          (Just (PostingList mempty p1 (NextPosting'Star (fmap mkNext ps)))))
          mempty cCloseCurly
          where
            mkNext p = NextPosting mempty cSemicolon mempty p

tracompriTopLine
  :: TxnParts
  -> AccValidation (NonEmpty TracompriError) (Maybe (TopLine Char ()))
tracompriTopLine parts@(TxnParts tl _) = case forest tl of
  AccFailure errs -> AccFailure . Pinchot.singleton
    $ TracompriBadForest parts errs
  AccSuccess mayFor -> case mayFor of
    Nothing -> pure Nothing
    Just for -> pure . Just . TopLine $ for

tracompriPosting
  :: TxnParts
  -- ^ The entire transaction.  Used only for error messages.
  -> (Seq Tree.Tree, Amount)
  -- ^ This posting
  -> AccValidation (NonEmpty TracompriError) (Posting Char ())
tracompriPosting parts (frst, amt) = case forest frst of
  AccFailure errs -> AccFailure . Pinchot.singleton
    $ TracompriBadForest parts errs
  AccSuccess mayForest -> AccSuccess
    $ Posting'TrioMaybeForest $ TrioMaybeForest tri mayWhitesBf
    where
      tri = amount amt
      mayWhitesBf = WhitesBracketedForest'Opt $ case mayForest of
        Nothing -> Nothing
        Just for -> Just (WhitesBracketedForest mempty (BracketedForest
          cOpenSquare mempty for mempty cCloseSquare))

tracompriPostings
  :: TxnParts
  -> AccValidation (NonEmpty TracompriError) (Seq (Posting Char ()))
tracompriPostings parts@(TxnParts _ pstgs)
  = traverse (tracompriPosting parts) pstgs

combineTracompris
  :: Seq (Maybe (FileItem Char ()))
  -> WholeFile Char ()
combineTracompris = toWholeFile . catMaybes
  where
    toWholeFile fis = WholeFile (WhitesFileItem'Star
      (fmap (WhitesFileItem mempty) fis)) mempty

wholeFile
  :: Seq Tracompri
  -> AccValidation (NonEmpty TracompriError) (WholeFile Char ())
wholeFile = fmap combineTracompris . traverse tracompri
