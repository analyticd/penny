{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Obtaining transactions and prices from a Copper-formatted file
-- takes three steps: parsing, decopperization, and proofing.  This
-- module performs proofing.
--
-- Proofing ensures that all transactions are balanced.
--
-- Proofing can fail.  If it does, the proofer tries to accumulate
-- as many error messages as possible, so that the user can fix all
-- errors at once rather than having to fix one error at a time.
--
-- Proofing has two steps.  The first step, called collecting, matches
-- up the fields in every posting with those that are allowed.
-- Collecting fails if a posting has duplicate fields.  This step is
-- necessary because the grammar allows posting fields to be presented
-- in any order, but does not check for duplicate posting fields.
-- Collecting is performed for an entire file at once, so all errors
-- from collecting are presented at once.
--
-- The second step, called validation, ensures that each transaction is
-- balanced.  It also ensures that the from and to commodities in each
-- price are different.

module Penny.Copper.Proofer where

import Penny.Commodity
import Penny.Copper.Decopperize
import Penny.Copper.PriceParts
import Penny.Copper.Tracompri
import Penny.Ents
import qualified Penny.Fields as F
import Penny.Price
import Penny.Tranche
import Penny.Transaction
import Penny.Trio

import Accuerr (Accuerr)
import qualified Accuerr
import qualified Control.Lens as Lens
import Control.Monad (foldM, (<=<))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Data.Sequence.NonEmpty as NE
import Sums (S3(S3_1, S3_2, S3_3))
import Data.Text (Text)

-- * Errors

data ValidationFailReason
  = FailedToAddTrio TrioError
  | Imbalanced ImbalancedError
  | MatchingFromTo Commodity
  deriving Show

data ProofFail r a = ProofFail
  { _location :: a
  , _reason :: r
  } deriving Show

Lens.makeLenses ''ProofFail

type CollectionFail = ProofFail PostingFieldDesc
type ValidationFail = ProofFail ValidationFailReason
type AccCollection a = Accuerr (NonEmptySeq (CollectionFail a))
type AccValidation a = Accuerr (NonEmptySeq (ValidationFail a))
type Proofer a = Either (Either (NonEmptySeq (CollectionFail a))
                                (NonEmptySeq (ValidationFail a)))

-- * Collection

postingFieldMap
  :: Foldable f
  => f (PostingFieldDecop a)
  -> Map PostingFieldDesc
         (NonEmptySeq (a, F.PostingFields -> F.PostingFields))
postingFieldMap = foldl f Map.empty
  where
    f acc (PostingFieldDecop loc desc mkr) = Map.alter (Just . g) desc acc
      where
        g Nothing = pair
        g (Just oldPairs) = oldPairs <> pair
        pair = NE.singleton (loc, mkr)

oneFieldOnly
  :: (PostingFieldDesc, NonEmptySeq (a, F.PostingFields -> F.PostingFields))
  -> AccCollection a (F.PostingFields -> F.PostingFields)
oneFieldOnly (desc, NE.NonEmptySeq (_, f1) fRest)
  = case NE.seqToNonEmptySeq fRest of
      Nothing -> Accuerr.AccSuccess f1
      Just ne -> Accuerr.AccFailure . fmap makeError $ ne
        where
          makeError (loc, _) = ProofFail loc desc


makePostingFields
  :: Foldable f
  => f (PostingFieldDecop a)
  -> AccCollection a F.PostingFields
makePostingFields
  = fmap (foldl (flip ($)) mempty)
  . traverse oneFieldOnly
  . Map.assocs
  . postingFieldMap

collectPostingFields
  :: PostingDecop a
  -> AccCollection a (a, Trio, F.PostingFields)
collectPostingFields (loc, tri, fieldDecops)
  = fmap f (makePostingFields fieldDecops)
  where
    f fields = (loc, tri, fields)

type Collected a = (a, F.TopLineFields, Seq (a, Trio, F.PostingFields))

collectTransactionPostingFields
  :: S3 a b (TxnParts loc)
  -> AccCollection loc (S3 a b (Collected loc))
collectTransactionPostingFields s3 = case s3 of
  S3_1 x -> pure (S3_1 x)
  S3_2 x -> pure (S3_2 x)
  S3_3 (loc, tlf, pstgDecops) ->
    fmap f . traverse collectPostingFields $ pstgDecops
    where
      f sq = S3_3 (loc, tlf, sq)

-- * Validation

addPostingToEnts
  :: Ents (Postline a)
  -> (a, Trio, F.PostingFields)
  -> Either (ValidationFail a) (Ents (Postline a))
addPostingToEnts ents (pos, trio, fields)
  = case appendTrio ents trio of
      Left e -> Left (ProofFail pos (FailedToAddTrio e))
      Right f -> Right $ f (Tranche pos fields)

-- | Creates an 'Balanced' from a sequence of postings.  If any single
-- posting fails, returns a single error message.  If balancing
-- fails, returns a single error message.
balancedFromPostings
  :: Seq (a, Trio, F.PostingFields)
  -> Either (ValidationFail a) (Balanced (Postline a))
balancedFromPostings sq = case NE.seqToNonEmptySeq sq of
  Nothing -> return mempty
  Just ne@(NE.NonEmptySeq (pos, _, _) _) ->
    case foldM addPostingToEnts mempty ne of
      Left e -> Left e
      Right g -> case entsToBalanced g of
        Left e -> Left (ProofFail pos (Imbalanced e))
        Right g -> return g

validatePrice
  :: PriceParts a
  -- ^
  -> Either (ValidationFail a) (Price a)
validatePrice pp = case makeFromTo (_priceFrom pp) (_priceTo pp) of
  Nothing -> Left (ProofFail (_pricePos pp) (MatchingFromTo (_priceFrom pp)))
  Just fromTo ->
    Right (Price (_priceTime pp) fromTo (_priceExch pp) (_pricePos pp))

validateItem
  :: S3 (PriceParts a) Text (Collected a)
  -> AccValidation a (Tracompri a)
validateItem s3 = case s3 of
  S3_1 p -> case validatePrice p of
    Left e -> Accuerr.AccFailure (NE.singleton e)
    Right g -> Accuerr.AccSuccess (Tracompri'Price g)
  S3_2 x -> pure (Tracompri'Comment x)
  S3_3 (loc, tlf, pstgs) -> case balancedFromPostings pstgs of
    Left e -> Accuerr.AccFailure (NE.singleton e)
    Right g -> pure (Tracompri'Transaction (Transaction (Tranche loc tlf) g))

-- * Proofing

proofItems
  :: Traversable t
  => t (S3 (PriceParts a) Text (TxnParts a))
  -> Proofer a (t (Tracompri a))
proofItems = proof <=< collect
  where
    collect = Lens.over Lens._Left Left
      . Accuerr.accuerrToEither
      . traverse collectTransactionPostingFields
    proof = Lens.over Lens._Left Right
      . Accuerr.accuerrToEither
      . traverse validateItem
