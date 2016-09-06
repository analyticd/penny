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
import Penny.Copper.Tracompri
import Penny.Copper.Types
import Penny.Decimal
import Penny.Ents
import qualified Penny.Fields as Fields
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

{-

-- # Troiload


-- # Amounts

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
-}
