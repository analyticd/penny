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

newtype Serial = Serial Unsigned
  deriving (Eq, Ord, Show)

newtype Forward = Forward Serial
  deriving (Eq, Ord, Show)

newtype Reverse = Reverse Serial
  deriving (Eq, Ord, Show)

data Serset = Serset Forward Reverse
  deriving (Eq, Ord, Show)

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

addSerials
  :: Seq (Seq (Transaction tm pm))
  -> Seq (Seq (Transaction (tm, TopLineSer) (pm, PostingSer)))
addSerials = undefined

makeForward :: State Unsigned Forward
makeForward = do
  this <- get
  modify next
  return $ Forward (Serial this)

makeReverse :: State Unsigned Reverse
makeReverse = do
  old <- get
  let new = case prev old of
        Nothing -> error "makeReverse: error"
        Just x -> x
  put new
  return $ Reverse (Serial new)


assignSingleTxnPostings
  :: Applicative m
  => m a
  -> Transaction tm pm
  -> m (Transaction tm (pm, a))
assignSingleTxnPostings fetch (Transaction tm pm)
  = Transaction tm <$> T.traverse (T.traverse k') pm
  where
    k' p = (,) <$> pure p <*> fetch

-- | Given a computation that assigns to a top line, assign to every
-- top line.
assignTopLine
  :: (Applicative m, T.Traversable t)
  => m a
  -> t (Transaction tm pm)
  -> m (t (Transaction (tm, a) pm))
assignTopLine fetch sq = T.traverse f sq
  where
    f (Transaction (TopLine ts m) p) = g <$> fetch
      where
        g m' = Transaction (TopLine ts (m, m')) p

-- | Given a computation that assigns to a posting, assign to every
-- posting.
assignPosting
  :: (Applicative m, T.Traversable t)
  => m a
  -> t (Transaction tm pm)
  -> m (t (Transaction tm (pm, a)))
assignPosting fetch sq = T.traverse f sq
  where
    f (Transaction tl p) = (Transaction tl) <$> inside
      where
        inside = T.traverse (T.traverse g) p
        g m = (,) <$> pure m <*> fetch

assignPostingIndex
  :: Transaction tm pm
  -> Transaction tm (pm, PostingIndex)
assignPostingIndex t = flip evalState (toUnsigned Zero) $ do
  withFwd <- assignSingleTxnPostings makeForward t
  withBack <- assignSingleTxnPostings makeReverse withFwd
  let f ((b, fwd), bak) = (b, PostingIndex (Serset fwd bak))
  return . second f $ withBack

assignPostingFileSer
  :: T.Traversable t1
  => t1 (Transaction tm pm)
  -> t1 (Transaction tm (pm, FileSer))
assignPostingFileSer t = flip evalState (toUnsigned Zero) $ do
  withFwd <- assignPosting makeForward t
  withRev <- assignPosting makeReverse withFwd
  let f ((b, fwd), bak) = (b, FileSer (Serset fwd bak))
  return . fmap (second f) $ withRev

assignPostingGlobalSer
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 (Transaction tm pm))
  -> t1 (t2 (Transaction tm (pm, GlobalSer)))
assignPostingGlobalSer t = flip evalState (toUnsigned Zero) $ do
  withFwd <- T.traverse (assignPosting makeForward) t
  withBak <- T.traverse (assignPosting makeReverse) withFwd
  let f ((b, fwd), bak) = (b, GlobalSer (Serset fwd bak))
  return . fmap (fmap (second f)) $ withBak

assignPostingSerials
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 (Transaction tm pm))
  -> t1 (t2 (Transaction tm (pm, PostingSer)))
assignPostingSerials t
  = fmap (fmap (second repack))
  . assignPostingGlobalSer
  . fmap assignPostingFileSer
  . fmap (fmap assignPostingIndex)
  $ t
  where
    repack (((pm, pidx), fileSer), glblSer)
      = (pm, PostingSer fileSer glblSer pidx)

assignTxnGlobalSer
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 (Transaction tm pm))
  -> t1 (t2 (Transaction (tm, GlobalSer) pm))
assignTxnGlobalSer t = flip evalState (toUnsigned Zero) $ do
  withFwd <- T.traverse (assignTopLine makeForward) t
  withBak <- T.traverse (assignTopLine makeReverse) withFwd
  let repack ((m, fwd), bak) = (m, GlobalSer (Serset fwd bak))
  return . fmap (fmap (first repack)) $ withBak

assignTxnFileSer
  :: T.Traversable t1
  => t1 (Transaction tm pm)
  -> t1 (Transaction (tm, FileSer) pm)
assignTxnFileSer t = flip evalState (toUnsigned Zero) $ do
  withFwd <- assignTopLine makeForward t
  withBak <- assignTopLine makeReverse withFwd
  let repack ((m, fwd), bak) = (m, FileSer (Serset fwd bak))
  return . fmap (first repack) $ withBak

assignTxnSerials
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 (Transaction tm pm))
  -> t1 (t2 (Transaction (tm, TopLineSer) pm))
assignTxnSerials
  = fmap (fmap (first repack))
  . fmap assignTxnFileSer
  . assignTxnGlobalSer
  where
    repack ((m, glbl), fle) = (m, TopLineSer fle glbl)

assignSerials
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 (Transaction tm pm))
  -> t1 (t2 (Transaction (tm, TopLineSer) (pm, PostingSer)))
assignSerials = assignTxnSerials . assignPostingSerials

