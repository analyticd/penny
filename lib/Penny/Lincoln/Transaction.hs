{-# LANGUAGE TupleSections #-}
module Penny.Lincoln.Transaction where
{-
  ( TopLine(..)
  , PstgMeta(..)
  , Transaction(..)
  , TransactionError(..)
  , transaction
  , Bundle(..)
  , transactionToBundles
  , bundleToTransaction
  , nextBundle
  , prevBundle
  , siblingBundles
  --, addSerials
  ) where
-}
import Penny.Lincoln.Ents
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Data.Sequence (Seq, viewl, ViewL(..))
import Data.Monoid
import qualified Data.Traversable as T
import Control.Monad.Trans.State
import qualified Data.Foldable as F
import Control.Applicative
import Penny.Lincoln.Natural
import Penny.Lincoln.Rep
import Data.Bifunctor
import Data.Bifoldable
import Penny.Lincoln.Serial

data TopLine a = TopLine [Tree] a
  deriving (Eq, Ord, Show)

instance Functor TopLine where
  fmap f (TopLine t a) = TopLine t (f a)

instance F.Foldable TopLine where
  foldr f z (TopLine _ a) = f a z

instance T.Traversable TopLine where
  traverse f (TopLine t a) = (TopLine t) <$> f a

data PstgMeta a = PstgMeta [Tree] Trio a
  deriving (Eq, Ord, Show)

instance Functor PstgMeta where
  fmap f (PstgMeta e i a) = PstgMeta e i (f a)

instance F.Foldable PstgMeta where
  foldr f z (PstgMeta _ _ a) = f a z

instance T.Traversable PstgMeta where
  traverse f (PstgMeta e i a) = (PstgMeta e i) <$> f a

instance Bifoldable Transaction where
  bifoldr fa fb z (Transaction (TopLine _ a) bal)
    = fa a
    . F.foldr fb z
    . fmap (\(PstgMeta _ _ m) -> m)
    $ bal

-- | A balanced set of postings, along with common metadata for all
-- the postings (often called the /top line data/, as it appears on
-- the top line of a checkbook register transaction.)
data Transaction tlMeta pMeta
  = Transaction (TopLine tlMeta) (Balanced (PstgMeta pMeta))
  -- ^ @Transaction a b c@, where
  --
  -- @a@ is the top line data
  --
  -- @b@ is arbitrary top line metadata
  --
  -- @b@ is the 'Balanced' set of postings; each of these may carry
  -- its own metadata.
  deriving (Eq, Ord, Show)

instance Bifunctor Transaction where
  bimap fa fb (Transaction t p) = Transaction (fmap fa t) (fmap (fmap fb) p)

data TransactionError a
  = BadTrio (PstgMeta a) TrioError
  -- ^ A particular 'Trio' could not create an 'Ent'.  Its
  -- accompanying metadata is also returned.
  | ImbalancedTransaction ImbalancedError
  -- ^ Each 'Trio' is satisfactory, but altogether they are not balanced.
  deriving (Eq, Ord, Show)

-- | Creates new 'Transaction'.  Fails if the input data is not
-- balanced or if one of the 'Trio' causes an error.
transaction
  :: TopLine tm
  -- ^ Top line data
  -> Seq (PstgMeta pm)
  -- ^ Each posting
  -> Either (TransactionError pm) (Transaction tm pm)
transaction topLine sqnce = makeEnts >>= makeTxn
  where
    makeEnts = go mempty sqnce
      where
        go soFar sq = case viewl sq of
          EmptyL -> return soFar
          pm@(PstgMeta _ tri _) :< xs -> case appendTrio soFar tri of
            Left e -> Left $ (BadTrio pm) e
            Right fn -> go (fn pm) xs

    makeTxn ents = case entsToBalanced ents of
      Left e -> Left $ ImbalancedTransaction e
      Right g -> Right $ Transaction topLine g


-- | A single posting, bundled with its sibling postings and with top
-- line metadata.
data Bundle tm pm = Bundle (TopLine tm) (View (PstgMeta pm))
  deriving (Eq, Ord, Show)

transactionToBundles :: Transaction tm pm -> Seq (Bundle tm pm)
transactionToBundles (Transaction tl bal) =
  fmap (Bundle tl) $ allViews bal


bundleToTransaction :: Bundle tm pm -> Transaction tm pm
bundleToTransaction (Bundle tl v) = Transaction tl (viewToBalanced v)

nextBundle :: Bundle tm pm -> Maybe (Bundle tm pm)
nextBundle (Bundle tl v) = fmap (Bundle tl) $ moveRight v

prevBundle :: Bundle tm pm -> Maybe (Bundle tm pm)
prevBundle (Bundle tl v) = fmap (Bundle tl) $ moveLeft v

siblingBundles :: Bundle tm pm -> Seq (Bundle tm pm)
siblingBundles (Bundle tl v) = fmap (Bundle tl) $ siblingViews v

newtype FileSer = FileSer Serset
  deriving (Eq, Ord, Show)

newtype GlobalSer = GlobalSer Serset
  deriving (Eq, Ord, Show)

data TopLineSer = TopLineSer FileSer GlobalSer
  deriving (Eq, Ord, Show)

data PostingIndex = PostingIndex Serset
  deriving (Eq, Ord, Show)

data PostingSer = PostingSer FileSer GlobalSer PostingIndex
  deriving (Eq, Ord, Show)

assignSingleTxnPostings
  :: (Applicative f, T.Traversable t1, T.Traversable t2)
  => f a
  -> (t, t1 (t2 a1))
  -> f (t, t1 (t2 (a1, a)))
assignSingleTxnPostings fetch (tm, pm)
  = (tm,) <$> T.traverse (T.traverse k') pm
  where
    k' p = (,) <$> pure p <*> fetch

-- | Given a computation that assigns to a top line, assign to every
-- top line.
assignTopLine
  :: (Applicative f, T.Traversable t2, T.Traversable t)
  => f a
  -> t (t2 a1, t1)
  -> f (t (t2 (a1, a), t1))
assignTopLine fetch sq = T.traverse f sq
  where
    f (tl, bal) = (,) <$> T.traverse g tl <*> pure bal
      where
        g m = (m,) <$> fetch

-- | Given a computation that assigns to a posting, assign to every
-- posting.
assignPosting
  :: (Applicative m, T.Traversable t, T.Traversable pm, T.Traversable bal)
  => m a
  -> t (tm, pm (bal mt))
  -> m (t (tm, pm (bal (mt, a))))
assignPosting fetch sq = T.traverse f sq
  where
    f (tl, p) = (tl,) <$> inside
      where
        inside = T.traverse (T.traverse g) p
        g m = (,) <$> pure m <*> fetch

{-
assignPostingIndex
  :: (T.Traversable pm, T.Traversable bal)
  => (tm, pm (bal mt))
  -> (tm, pm (bal (mt, PostingIndex)))
assignPostingIndex t = flip evalState (toUnsigned Zero) $ do
  withFwd <- assignSingleTxnPostings makeForward t
  withBack <- assignSingleTxnPostings makeReverse withFwd
  let f ((b, fwd), bak) = (b, PostingIndex (Serset fwd bak))
  return . second (fmap (fmap f)) $ withBack
-}
assignPostingIndex
  :: (T.Traversable pm, T.Traversable bal)
  => (tm, pm (bal mt))
  -> (tm, pm (bal (mt, PostingIndex)))
assignPostingIndex
  = second (fmap (fmap (second PostingIndex)))
  . second serialNumbersNested

assignPostingFileSer
  :: (T.Traversable t1, T.Traversable bal, T.Traversable pm)
  => t1 (tm, pm (bal mt))
  -> t1 (tm, pm (bal (mt, FileSer)))
assignPostingFileSer t = flip evalState (toUnsigned Zero) $ do
  withFwd <- assignPosting makeForward t
  withRev <- assignPosting makeReverse withFwd
  let f ((b, fwd), bak) = (b, FileSer (Serset fwd bak))
  return . fmap (second (fmap (fmap f))) $ withRev

assignPostingGlobalSer
  :: (T.Traversable t1, T.Traversable t2, T.Traversable pm, T.Traversable bal)
  => t1 (t2 (tm, pm (bal mt)))
  -> t1 (t2 (tm, pm (bal (mt, GlobalSer))))
assignPostingGlobalSer t = flip evalState (toUnsigned Zero) $ do
  withFwd <- T.traverse (assignPosting makeForward) t
  withBak <- T.traverse (assignPosting makeReverse) withFwd
  let f ((b, fwd), bak) = (b, GlobalSer (Serset fwd bak))
  return . fmap (fmap (second (fmap (fmap f)))) $ withBak

assignPostingSerials
  :: (T.Traversable t1, T.Traversable t2, T.Traversable pm, T.Traversable bal)
  => t1 (t2 (tm, pm (bal mt)))
  -> t1 (t2 (tm, pm (bal (mt, PostingSer))))
assignPostingSerials t
  = fmap (fmap (second (fmap (fmap repack))))
  . assignPostingGlobalSer
  . fmap assignPostingFileSer
  . fmap (fmap assignPostingIndex)
  $ t
  where
    repack (((pm, pidx), fileSer), glblSer)
      = (pm, PostingSer fileSer glblSer pidx)

assignTxnGlobalSer
  :: (T.Traversable t1, T.Traversable t2, T.Traversable tm)
  => t1 (t2 (tm mt, pm))
  -> t1 (t2 (tm (mt, GlobalSer), pm))
assignTxnGlobalSer t = flip evalState (toUnsigned Zero) $ do
  withFwd <- T.traverse (assignTopLine makeForward) t
  withBak <- T.traverse (assignTopLine makeReverse) withFwd
  let repack ((m, fwd), bak) = (m, GlobalSer (Serset fwd bak))
  return . fmap (fmap (first (fmap repack))) $ withBak

assignTxnFileSer
  :: (T.Traversable t1, T.Traversable tm)
  => t1 (tm mt, pm)
  -> t1 ((tm (mt, FileSer)), pm)
assignTxnFileSer t = flip evalState (toUnsigned Zero) $ do
  withFwd <- assignTopLine makeForward t
  withBak <- assignTopLine makeReverse withFwd
  let repack ((m, fwd), bak) = (m, FileSer (Serset fwd bak))
  return . fmap (first (fmap repack)) $ withBak

assignTxnSerials
  :: (T.Traversable t1, T.Traversable t2, T.Traversable tm)
  => t1 (t2 (tm mt, pm))
  -> t1 (t2 (tm (mt, TopLineSer), pm))
assignTxnSerials
  = fmap (fmap (first (fmap repack)))
  . fmap assignTxnFileSer
  . assignTxnGlobalSer
  where
    repack ((m, glbl), fle) = (m, TopLineSer fle glbl)

assignSerials
  :: (T.Traversable t1, T.Traversable t2, T.Traversable tm,
      T.Traversable bal, T.Traversable pm)
  => t1 (t2 (tm mtm, pm (bal mt)))
  -> t1 (t2 (tm (mtm, TopLineSer), pm (bal (mt, PostingSer))))
assignSerials = assignTxnSerials . assignPostingSerials

assignSerialsToTxn
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 (Transaction tlMeta pMeta))
  -> t1 (t2 (Transaction (tlMeta, TopLineSer) (pMeta, PostingSer)))
assignSerialsToTxn
  = fmap (fmap (uncurry Transaction))
  . assignSerials
  . fmap (fmap (\(Transaction a b) -> (a, b)))
