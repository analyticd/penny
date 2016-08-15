{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Obtaining transactions and prices from a Copper-formatted file
-- takes three steps: parsing, decopperization, and proofing.  This
-- module performs proofing.
--
-- Proofing ensures that all transactions are balanced.  Also, the
-- Copper format is loose in the sense that the grammar does not
-- specify the various fields that are found in "Penny.Fields".
-- Proofing uses various conventions to assign different trees to
-- different fields.
--
-- Proofing can fail.  If it does, the proofer tries to accumulate
-- as many error messages as possible, so that the user can fix all
-- errors at once rather than having to fix one error at a time.

module Penny.Copper.Proofer where

import Penny.Commodity
import Penny.Copper.Decopperize
import Penny.Copper.PriceParts
import Penny.Copper.Tracompri
import Penny.Ents
import qualified Penny.Fields as F
import Penny.Price
import Penny.Scalar
import Penny.SeqUtil
import Penny.Tranche
import Penny.Transaction
import Penny.Trio
import Penny.Tree
import Penny.Tree.Harvester

import Accuerr (Accuerr)
import qualified Accuerr
import qualified Control.Lens as Lens
import Control.Monad (foldM, guard)
import qualified Control.Monad.Trans.State as St
import Data.Maybe (fromMaybe, isNothing)
import Data.OFX (TrnType)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Data.Sequence.NonEmpty as NE
import Data.Sums (S3(S3a, S3b, S3c))
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Time as Time
import Text.Read (readMaybe)

findDay
  :: Tree
  -> Maybe Time.Day
findDay x = childlessTree x >>= Lens.preview _SDay

findTime :: Tree -> Maybe Time.TimeOfDay
findTime x = childlessTree x >>= Lens.preview _STime

findZone :: Tree -> Maybe Time.TimeZone
findZone x = childlessTree x >>= Lens.preview _SZone
  >>= return . Time.minutesToTimeZone

findPayee :: Tree -> Maybe Text
findPayee x = childlessTree x >>= Lens.preview _SText

-- | If this tree is labeled @fitid@ and has a single child tree
-- that is also a childless tree whose scalar is an SText,
-- return that SText.
findOrigPayee :: Tree -> Maybe Text
findOrigPayee t = do
  children <- labeledTree "origPayee" t
  child <- isSingleton children
  scalar <- childlessTree child
  Lens.preview _SText scalar

getTopLineFields
  :: a
  -- ^ Location
  -> Seq Tree
  -> Either (ProofFail a) (F.TopLineFields, Seq Tree)
getTopLineFields loc forest = case mayTlf of
  Nothing -> Left (ProofFail loc UndatedTransaction)
  Just tlf -> Right tlf
  where
    ((mayPye, mayOrig, mayZt), forest') = St.runState k forest
    mayTlf = fmap (\zt -> (F.TopLineFields zt mayPye mayOrig, forest')) mayZt
    k = do
      mayDay <- yankSt findDay
      mayTime <- yankSt findTime
      mayZone <- yankSt findZone
      mayPayee <- yankSt findPayee
      mayOrigPayee  <- yankSt findOrigPayee
      return (mayPayee, mayOrigPayee, getZonedTime mayDay mayTime mayZone)
    getZonedTime mayDay mayTime mayZone = fmap mkZt mayDay
      where
        mkZt day = Time.ZonedTime lt tz
          where
            lt = Time.LocalTime day tod
              where
                tod = fromMaybe Time.midnight mayTime
            tz = fromMaybe Time.utc mayZone

findNumber :: Tree -> Maybe Integer
findNumber x = childlessTree x >>= Lens.preview _SInteger

findFlag :: Tree -> Maybe Text
findFlag = findPayee

-- | Gets a tree that has the particular label and retrieves the
-- scalar that can be fetched with the given prism.
labeledChild
  :: Text
  -- ^ Label for the parent tree
  -> Lens.Prism' Scalar a
  -> Tree
  -> Maybe a
labeledChild lbl prism tree = do
  children <- labeledTree lbl tree
  child <- isSingleton children
  scalar <- childlessTree child
  Lens.preview prism scalar

-- | If this tree has no scalar and all child trees have a 'SText'
-- scalar, and there is at least one child tree, return those
-- scalars.
findAccount :: Tree -> Maybe (NonEmptySeq Text)
findAccount t = do
  guard . isNothing . _scalar $ t
  scalars <- traverse childlessTree . _children $ t
  texts <- traverse (Lens.preview _SText) scalars
  NE.seqToNonEmptySeq texts

-- | If this tree is labeled @fitid@ and has a single child tree
-- that is also a childless tree whose scalar is an SText,
-- return that SText.
findFitid :: Tree -> Maybe Text
findFitid = labeledChild "fitid" _SText

-- | If this tree is labeled @uid@ and has a single child tree that is
-- also a childless tree whose scalar is an SText, return that SText.
findUid :: Tree -> Maybe Text
findUid = labeledChild "uid" _SText

-- | Finds a tree labeled @trnType@ that has a single child tree that
-- is also a childless tree whose scalar is an SText and that, in
-- addition, can be 'read' as a 'TrnType'.
findTrnType :: Tree -> Maybe TrnType
findTrnType t = labeledChild "trnType" _SText t >>= (readMaybe . X.unpack)

-- | Finds a tree labeled @origDay@ with a 'Time.Day'.
findOrigDay :: Tree -> Maybe Time.Day
findOrigDay = labeledChild "origDay" _SDay

findOrigTime :: Tree -> Maybe Time.TimeOfDay
findOrigTime = labeledChild "origTime" _STime

findOrigZone :: Tree -> Maybe Int
findOrigZone = labeledChild "origZone" _SZone

-- | If this tree is labeled @tags@ and each child tree is also a
-- childless tree with a 'SText' scalar, and there is at least
-- one child tree, return those scalars.
findTags :: Tree -> Maybe (NonEmptySeq Text)
findTags t = do
  children <- labeledTree "tags" t
  scalars <- traverse childlessTree children
  texts <- traverse (Lens.preview _SText) scalars
  NE.seqToNonEmptySeq texts

getPostingFields :: Seq Tree -> (F.PostingFields, Seq Tree)
getPostingFields = St.runState k
  where
    k = F.PostingFields <$> yankSt findNumber
      <*> yankSt findFlag
      <*> fmap (maybe Seq.empty NE.nonEmptySeqToSeq) (yankSt findAccount)
      <*> yankSt findFitid
      <*> fmap (maybe Seq.empty NE.nonEmptySeqToSeq) (yankSt findTags)
      <*> yankSt findUid
      <*> yankSt findTrnType
      <*> yankSt findOrigDay
      <*> yankSt findOrigTime
      <*> yankSt findOrigZone


data Reason
  = FailedToAddTrio TrioError
  | Imbalanced ImbalancedError
  | MatchingFromTo Commodity
  | UndatedTransaction
  deriving Show

data ProofFail a = ProofFail
  { _location :: a
  , _reason :: Reason
  } deriving Show

Lens.makeLenses ''ProofFail

addPostingToEnts
  :: Ents (Postline a)
  -> (a, Trio, Seq Tree)
  -- ^ Location, Trio, forest
  -> Either (ProofFail a) (Ents (Postline a))
addPostingToEnts ents (pos, trio, trees)
  = case appendTrio ents trio of
      Left e -> Left (ProofFail pos (FailedToAddTrio e))
      Right f -> Right $ f (Tranche pos anc fields)
        where
          (fields, anc) = getPostingFields trees

-- | Creates an 'Balanced' from a sequence of postings.  If any single
-- posting fails, returns a single error message.  If balancing
-- fails, returns a single error message.
balancedFromPostings
  :: Seq (a, Trio, Seq Tree)
  -- ^ Location, trio, forest
  -> Either (ProofFail a) (Balanced (Postline a))
balancedFromPostings sq = case NE.seqToNonEmptySeq sq of
  Nothing -> return mempty
  Just ne@(NE.NonEmptySeq (pos, _, _) _) ->
    case foldM addPostingToEnts mempty ne of
      Left e -> Left e
      Right g -> case entsToBalanced g of
        Left e -> Left (ProofFail pos (Imbalanced e))
        Right g -> return g

price
  :: PriceParts a
  -- ^
  -> Either (ProofFail a) (Price a)
price pp = case makeFromTo (_priceFrom pp) (_priceTo pp) of
  Nothing -> Left (ProofFail (_pricePos pp) (MatchingFromTo (_priceFrom pp)))
  Just fromTo ->
    Right (Price (_priceTime pp) fromTo (_priceExch pp) (_pricePos pp))

proofItem
  :: S3 (PriceParts a) Text (TxnParts a)
  -> Accuerr (NonEmptySeq (ProofFail a)) (Tracompri a)
proofItem s3 = case s3 of
  S3a p -> case price p of
    Left e -> Accuerr.AccFailure (NE.singleton e)
    Right g -> Accuerr.AccSuccess (Tracompri'Price g)
  S3b x -> pure (Tracompri'Comment x)
  S3c (loc, topLine, pstgs) -> fmap Tracompri'Transaction
    $ Transaction <$> tlf <*> bal
    where
      tlf = case getTopLineFields loc topLine of
        Left e -> Accuerr.AccFailure (NE.singleton e)
        Right (fields, aux) -> Accuerr.AccSuccess
          (Tranche loc aux fields)
      bal = case balancedFromPostings pstgs of
        Left e -> Accuerr.AccFailure (NE.singleton e)
        Right g -> Accuerr.AccSuccess g

proofItems
  :: Seq (S3 (PriceParts a) Text (TxnParts a))
  -> Accuerr (NonEmptySeq (ProofFail a)) (Seq (Tracompri a))
proofItems = traverse proofItem
