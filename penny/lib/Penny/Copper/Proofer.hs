{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Obtaining transactions and prices from a Copper-formatted file
-- takes three steps: parsing, decopperization, and proofing.  This
-- module performs proofing.
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
import Penny.Transaction
import Penny.Trio
import Penny.Tree
import Penny.Tree.Harvester

import qualified Control.Lens as Lens
import Control.Monad (foldM)
import qualified Control.Monad.Trans.State as St
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Validation as V
import Pinchot (Loc)

findDay
  :: Tree
  -> Maybe Time.Day
findDay x = childlessUserTree x >>= Lens.preview _SDay

findTime :: Tree -> Maybe Time.TimeOfDay
findTime x = childlessUserTree x >>= Lens.preview _STime

findZone :: Tree -> Maybe Time.TimeZone
findZone x = childlessUserTree x >>= Lens.preview _SZone
  >>= return . Time.minutesToTimeZone

findPayee :: Tree -> Maybe Text
findPayee x = childlessUserTree x >>= Lens.preview _SText

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
findNumber x = childlessUserTree x >>= Lens.preview _SInteger

findFlag :: Tree -> Maybe Text
findFlag = findPayee

-- | If this tree is a User tree where all child User trees are
-- childless user trees with a 'SText' scalar, and there is at least
-- one child User tree, return those scalars.
findAccount :: Tree -> Maybe (NonEmpty Text)
findAccount t = do
  scalars <- traverse childlessUserTree . _children $ t
  texts <- traverse (Lens.preview _SText) scalars
  nonEmpty texts

-- | If this tree is labeled @fitid@ and has a single child tree
-- that is also a childless user tree whose scalar is an SText,
-- return that SText.
findFitid :: Tree -> Maybe Text
findFitid t = do
  children <- labeledTree "fitid" t
  child <- isSingleton children
  scalar <- childlessUserTree child
  Lens.preview _SText scalar

-- | If this tree is labeled @tags@ and each child tree is also a
-- childless user tree with a 'SText' scalar, and there is at least
-- one child User tree, return those scalars.
findTags :: Tree -> Maybe (NonEmpty Text)
findTags t = do
  children <- labeledTree "tags" t
  scalars <- traverse childlessUserTree children
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
      Right f -> Right $ f (TopLineOrPostline pos anc fields)
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
  -> V.AccValidation (NonEmpty ProofFail) (Either Price (Transaction Loc))
proofItem x = case x of
  Left p -> case price p of
    Left e -> V.AccFailure (singleton e)
    Right g -> V.AccSuccess (Left g)
  Right (loc, topLine, pstgs) -> fmap Right $ Transaction <$> tlf <*> bal
    where
      tlf = case getTopLineFields loc topLine of
        Left e -> V.AccFailure (singleton e)
        Right (fields, aux) -> V.AccSuccess
          (TopLineOrPostline loc aux fields)
      bal = case balancedFromPostings pstgs of
        Left e -> V.AccFailure (singleton e)
        Right g -> V.AccSuccess g

proofItems
  :: Seq (Either (PriceParts Loc) TxnParts)
  -- ^
  -> V.AccValidation (NonEmpty ProofFail) (Seq (Either Price (Transaction Loc)))
proofItems = traverse proofItem
