{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Penny.Ents
import qualified Penny.Fields as F
import Penny.NonEmpty
import Penny.Price
import Penny.Scalar
import Penny.SeqUtil
import Penny.Tranche
import Penny.TransactionBare
import Penny.Trio
import Penny.Tree
import Penny.Tree.Harvester

import qualified Control.Lens as Lens
import Control.Monad (foldM, guard)
import qualified Control.Monad.Trans.State as St
import Data.Maybe (fromMaybe, isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Time as Time
import Accuerr (Accuerr)
import qualified Accuerr
import Pinchot (Loc)

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

getTopLineFields
  :: Loc
  -> Seq Tree
  -> Either ProofFail (F.TopLineFields, Seq Tree)
getTopLineFields loc forest = case mayTlf of
  Nothing -> Left (ProofFail loc UndatedTransaction)
  Just tlf -> Right tlf
  where
    ((mayPye, mayZt), forest') = St.runState k forest
    mayTlf = fmap (\zt -> (F.TopLineFields zt mayPye, forest')) mayZt
    k = do
      mayDay <- yankSt findDay
      mayTime <- yankSt findTime
      mayZone <- yankSt findZone
      mayPayee <- yankSt findPayee
      return (mayPayee, getZonedTime mayDay mayTime mayZone)
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

-- | If this tree has no scalar and all child trees have a 'SText'
-- scalar, and there is at least one child tree, return those
-- scalars.
findAccount :: Tree -> Maybe (NonEmpty Text)
findAccount t = do
  guard . isNothing . _scalar $ t
  scalars <- traverse childlessTree . _children $ t
  texts <- traverse (Lens.preview _SText) scalars
  nonEmpty texts

-- | If this tree is labeled @fitid@ and has a single child tree
-- that is also a childless tree whose scalar is an SText,
-- return that SText.
findFitid :: Tree -> Maybe Text
findFitid t = do
  children <- labeledTree "fitid" t
  child <- isSingleton children
  scalar <- childlessTree child
  Lens.preview _SText scalar

-- | If this tree is labeled @uid@ and has a single child tree that is
-- also a childless tree whose scalar is an SText, return that SText.
findUid :: Tree -> Maybe Text
findUid t = do
  children <- labeledTree "uid" t
  child <- isSingleton children
  scalar <- childlessTree child
  Lens.preview _SText scalar


-- | If this tree is labeled @tags@ and each child tree is also a
-- childless tree with a 'SText' scalar, and there is at least
-- one child tree, return those scalars.
findTags :: Tree -> Maybe (NonEmpty Text)
findTags t = do
  children <- labeledTree "tags" t
  scalars <- traverse childlessTree children
  texts <- traverse (Lens.preview _SText) scalars
  nonEmpty texts

getPostingFields :: Seq Tree -> (F.PostingFields, Seq Tree)
getPostingFields = St.runState k
  where
    k = F.PostingFields <$> yankSt findNumber
      <*> yankSt findFlag
      <*> fmap (maybe Seq.empty seqFromNonEmpty) (yankSt findAccount)
      <*> yankSt findFitid
      <*> fmap (maybe Seq.empty seqFromNonEmpty) (yankSt findTags)
      <*> yankSt findUid


data Reason
  = FailedToAddTrio TrioError
  | Imbalanced ImbalancedError
  | MatchingFromTo Commodity
  | UndatedTransaction
  deriving Show

data ProofFail = ProofFail
  { _location :: Loc
  , _reason :: Reason
  } deriving Show

Lens.makeLenses ''ProofFail

addPostingToEnts
  :: Ents (Postline Loc)
  -> (Loc, Trio, Seq Tree)
  -> Either ProofFail (Ents (Postline Loc))
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
  :: Seq (Loc, Trio, Seq Tree)
  -> Either ProofFail (Balanced (Postline Loc))
balancedFromPostings sq = case nonEmpty sq of
  Nothing -> return mempty
  Just ne@(NonEmpty (pos, _, _) _) -> case foldM addPostingToEnts mempty ne of
    Left e -> Left e
    Right g -> case entsToBalanced g of
      Left e -> Left (ProofFail pos (Imbalanced e))
      Right g -> return g

price
  :: PriceParts Loc
  -- ^
  -> Either ProofFail Price
price pp = case makeFromTo (FromCy (_priceFrom pp)) (ToCy (_priceTo pp)) of
  Nothing -> Left (ProofFail (_pricePos pp) (MatchingFromTo (_priceFrom pp)))
  Just fromTo -> Right (Price (_priceTime pp) fromTo (_priceExch pp))

proofItem
  :: Either (PriceParts Loc) TxnParts
  -- ^
  -> Accuerr (NonEmpty ProofFail) (Either Price (TransactionBare Loc))
proofItem x = case x of
  Left p -> case price p of
    Left e -> Accuerr.AccFailure (singleton e)
    Right g -> Accuerr.AccSuccess (Left g)
  Right (loc, topLine, pstgs) -> fmap Right $ TransactionBare <$> tlf <*> bal
    where
      tlf = case getTopLineFields loc topLine of
        Left e -> Accuerr.AccFailure (singleton e)
        Right (fields, aux) -> Accuerr.AccSuccess
          (Tranche loc aux fields)
      bal = case balancedFromPostings pstgs of
        Left e -> Accuerr.AccFailure (singleton e)
        Right g -> Accuerr.AccSuccess g

proofItems
  :: Seq (Either (PriceParts Loc) TxnParts)
  -- ^
  -> Accuerr (NonEmpty ProofFail) (Seq (Either Price (TransactionBare Loc)))
proofItems = traverse proofItem
