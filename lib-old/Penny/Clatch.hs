{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
-- | A 'Clatch' is a single posting from a collection of postings,
-- after considerable processing.  Here are the processing steps:
--
-- 1. Begin with all the transactions returned by 'vault'.
--
-- 2. Convert the amounts.  This allows reports to show both the
-- original amount and an arbitrary amount resulting from a
-- conversion.
--
-- 3. Create a map, 'Renderings', that describes how amounts are
-- rendered, if there is enough information in the 'Trio' to determine
-- this.  This map allows reports to format their numbers based on a
-- histogram of how they have been formatted before.
--
-- 4. Filter the postings.  This way the user can include in the
-- report only the postings she is interested in.
--
-- 5. Sort the postings.
--
-- 6. Compute a running balance of the sorted postings.
--
-- 7. Filter the sorted postings.  This allows the user to see a
-- report that has a running balance that includes an entire set of
-- postings, but that only shows postings of interest.

module Penny.Clatch where

import Control.Lens
import Control.Monad hiding (filterM)
import Penny.Converted
import Penny.Ledger (TreeL, Ledger)
import qualified Penny.Ledger
import Penny.SeqUtil
import Penny.Transbox
import Penny.Viewpost
import Penny.Sorted
import Penny.Filtered
import Data.Sequence (Seq)
import Data.Monoid
import Penny.Balance
import Penny.Serial
import qualified Data.Traversable as T

type Clatch
  = Transbox (Viewpost (Converted (Filtered (Sorted
      (RunningBalance (Filtered ()))))))

pstgMeta :: Transbox (Viewpost a) -> Ledger (Seq TreeL)
pstgMeta = Penny.Ledger.pstgMeta . view (transboxee.viewpost.onView)

allMeta :: Transbox (Viewpost a) -> Ledger (Seq TreeL)
allMeta t = liftM2 mappend (pstgMeta t) (txnMeta t)


createViewposts :: Transbox a -> Ledger (Seq (Transbox (Viewpost ())))
createViewposts tbox
  = return . fmap (\vw -> Transbox (_transaction tbox) (Viewpost vw ()))
           . allViews
  <=< Penny.Ledger.postings . _transaction
  $ tbox

createConverted
  :: Traversable t
  => Converter
  -> t (Viewpost a)
  -> Ledger (t (Viewpost (Converted ())))
createConverted converter = traverse f
  where
    f (Viewpost vw _) = do
      converted <- convertPosting converter (_onView vw)
      let converted' = () <$ converted
      return $ Viewpost vw converted'

createFiltered
  :: (Transbox (Viewpost (Converted ())) -> Ledger Bool)
  -- ^ Predicate
  -> Seq (Transbox (Viewpost (Converted ())))
  -> Ledger (Seq (Transbox (Viewpost (Converted (Filtered ())))))
createFiltered pd = fmap f . filterM pd
  where
    f = fmap rewrap . assignSersetted
    rewrap (Sersetted srst tb) = tb & transboxee.viewpostee.convertee
      .~ Sersetted srst ()

createSorted
  :: Sorter Ledger (Transbox (Viewpost (Converted (Filtered ()))))
  -> Seq (Transbox (Viewpost (Converted (Filtered ()))))
  -> Ledger (Seq (Transbox (Viewpost (Converted (Filtered (Sorted ()))))))
createSorted srtr = fmap (fmap f) . sortWithSersetted srtr
  where
    f (Sersetted srst tb) = tb & transboxee.viewpostee.convertee.sersetee
        .~ Sersetted srst ()


createRunningBalance
  :: Seq (Transbox (Viewpost (Converted (Filtered (Sorted ())))))
  -> Ledger (Seq (Transbox (Viewpost (Converted (Filtered (Sorted
               (RunningBalance ())))))))
createRunningBalance
  = return . snd . T.mapAccumL addBal mempty
  <=< traverse addAmount
  where
    addAmount tb = do
      am <- bestAmount (tb ^. transboxee)
      return $ tb & transboxee.viewpostee.convertee.sersetee.sersetee
        .~ am
    addBal acc tb = (acc', new)
      where
        acc' = acc <> c'Balance'Amount
          (tb ^. transboxee.viewpostee.convertee.sersetee.sersetee)
        new = tb & transboxee.viewpostee.convertee.sersetee.sersetee
          .~ RunningBalance acc' ()

filterBalances
  :: (Transbox (Viewpost (Converted (Filtered (Sorted
       (RunningBalance ()))))) -> Ledger Bool)
  -- ^ Predicate
  -> Seq (Transbox (Viewpost (Converted (Filtered (Sorted
          (RunningBalance ()))))))
  -> Ledger (Seq Clatch)
filterBalances pd = fmap f . filterM pd
  where
    f = fmap rewrap . assignSersetted
    rewrap (Sersetted srst tb) = tb
      & transboxee.viewpostee.convertee.sersetee.sersetee.runningBalancee
          .~ Sersetted srst ()

clatches
  :: Converter
  -> (Transbox (Viewpost (Converted ())) -> Ledger Bool)
  -> Sorter Ledger (Transbox (Viewpost (Converted (Filtered ()))))
  -> (Transbox (Viewpost (Converted (Filtered (Sorted
      (RunningBalance ()))))) -> Ledger Bool)
  -> Seq Penny.Ledger.TransactionL
  -> Ledger (Seq Clatch)
clatches converter pdConverted sorter pdSorted
  =   filterBalances pdSorted
  <=< createRunningBalance
  <=< createSorted sorter
  <=< createFiltered pdConverted
  <=< traverse (createConverted converter)
  <=< fmap join . traverse createViewposts
  <=< return . fmap (\txn -> Transbox txn ())

