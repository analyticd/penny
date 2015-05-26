{-# LANGUAGE OverloadedStrings #-}

module Penny.Clatch.Matcher where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Class
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Penny.Clatch
import Penny.Ledger
import Penny.Ledger.Matcher (hasOffspring)
import qualified Penny.Ledger.Matcher
import Penny.Transbox
import Penny.SeqUtil
import Penny.Matcher
import Penny.Viewpost
import Penny.Field.Matcher
import Turtle.Pattern (prefix, suffix)

{-

studyTxnMeta :: Ledger l => Matcher (TreeL l) l a -> Matcher (Clatch l) l a
studyTxnMeta mr = do
  subj <- getSubject
  trees <- lift . txnMeta . transactionL $ subj
  flip study trees . each $ mr

studyPstgMeta :: Ledger l => Matcher (TreeL l) l a -> Matcher (Clatch l) l a
studyPstgMeta mr = do
  subj <- getSubject
  trees <- lift . pstgMeta . postingL $ subj
  flip study trees . each $ mr

studyBothMeta :: Ledger l => Matcher (TreeL l) l a -> Matcher (Clatch l) l a
studyBothMeta mr = studyPstgMeta mr <|> studyTxnMeta mr

-- | Creates a 'Matcher' that looks for a parent tree with the exact
-- name given.  First performs a pre-order search in the metadata of
-- the posting; then performs a pre-order search in the metadata for
-- the top line.  If successful, returns the child forest.
findNamedTree
  :: Ledger l
  => Text
  -> Matcher (Clatch l) l (Seq (TreeL l))
findNamedTree name = studyBothMeta $ do
  sc <- Penny.Ledger.Matcher.scalar
  txt <- study Penny.Field.Matcher.text sc
  study (equal name) txt
  subj <- getSubject
  lift $ Penny.Ledger.offspring subj

payee :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
payee = (findNamedTree "payee" <|>) . studyTxnMeta $ do
  invert hasOffspring
  sc <- Penny.Ledger.Matcher.scalar
  txt <- study text sc
  invert $ study (pattern (prefix "(" >> suffix ")")) txt
  fmap Seq.singleton getSubject

account :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
account = (findNamedTree "account" <|>) . studyPstgMeta $ do
  invert Penny.Ledger.Matcher.scalar
  hasOffspring
  getSubject >>= lift . Penny.Ledger.offspring

tags :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
tags = (findNamedTree "tags" <|>) . studyTxnMeta $ do
  invert Penny.Ledger.Matcher.scalar
  hasOffspring
  getSubject >>= lift . Penny.Ledger.offspring

flag :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
flag = (findNamedTree "flag" <|>) . studyBothMeta $ do
  txt <- Penny.Ledger.Matcher.scalar >>= study text
  study (pattern (prefix "(" >> suffix ")")) txt
  invert hasOffspring
  fmap Seq.singleton getSubject

number :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
number = (findNamedTree "number" <|>) . studyBothMeta $ do
  Penny.Ledger.Matcher.scalar >>= study integer
  invert hasOffspring
  fmap Seq.singleton getSubject

date :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
date = (findNamedTree "date" <|>) . studyTxnMeta $ do
  Penny.Ledger.Matcher.scalar >>= study Penny.Field.Matcher.date
  invert hasOffspring
  fmap Seq.singleton getSubject

time :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
time = (findNamedTree "time" <|>) . studyTxnMeta $ do
  Penny.Ledger.Matcher.scalar >>= study Penny.Field.Matcher.time
  invert hasOffspring
  fmap Seq.singleton getSubject


zone :: Ledger l => Matcher (Clatch l) l (Seq (TreeL l))
zone = (findNamedTree "zone" <|>) . studyTxnMeta $ do
  Penny.Ledger.Matcher.scalar >>= study Penny.Field.Matcher.zone
  invert hasOffspring
  fmap Seq.singleton getSubject


-}
