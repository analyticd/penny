module Penny.Lincoln.Transaction where

import Penny.Lincoln.Ents
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Data.Map (Map)
import Data.Sequence (Seq, viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Monoid
import qualified Data.Traversable as T
import Control.Monad.Trans.State
import qualified Data.Map as M
import Data.Text (Text)

newtype TopLine = TopLine Forest
  deriving (Eq, Ord, Show)

data PstgMeta = PstgMeta Forest Trio
  deriving (Eq, Ord, Show)

-- | A balanced set of postings, along with common metadata for all
-- the postings (often called the /top line data/, as it appears on
-- the top line of a checkbook register transaction.)
data Transaction
  = Transaction TopLine (Balanced PstgMeta)
  -- ^ @Transaction a b@, where
  --
  -- @a@ is the top line data
  --
  -- @b@ is the 'Balanced' set of postings; each of these may carry
  -- its own metadata.
  deriving (Eq, Ord, Show)

data TransactionError
  = BadTrio PstgMeta TrioError
  -- ^ A particular 'Trio' could not create an 'Ent'.  Its
  -- accompanying metadata is also returned.
  | ImbalancedTransaction ImbalancedError
  -- ^ Each 'Trio' is satisfactory, but altogether they are not balanced.
  deriving (Eq, Ord, Show)

-- | Creates new 'Transaction'.  Fails if the input data is not
-- balanced or if one of the 'Trio' causes an error.
transaction
  :: TopLine
  -- ^ Top line data
  -> Seq PstgMeta
  -- ^ Each posting
  -> Either TransactionError Transaction
transaction topLine sqnce = makeEnts >>= makeTxn
  where
    makeEnts = go mempty sqnce
      where
        go soFar sq = case viewl sq of
          EmptyL -> return soFar
          pm@(PstgMeta _ tri) :< xs -> case appendTrio soFar tri of
            Left e -> Left $ (BadTrio pm) e
            Right fn -> go (fn pm) xs

    makeTxn ents = case entsToBalanced ents of
      Left e -> Left $ ImbalancedTransaction e
      Right g -> Right $ Transaction topLine g

-- | A single posting, bundled with its sibling postings and with top
-- line metadata.
data Bundle = Bundle TopLine (View PstgMeta)
  deriving (Eq, Ord, Show)

transactionToBundles :: Transaction -> Seq Bundle
transactionToBundles (Transaction tl bal) =
  fmap (Bundle tl) $ allViews bal

bundleToTransaction :: Bundle -> Transaction
bundleToTransaction (Bundle tl v) = Transaction tl (viewToBalanced v)

nextBundle :: Bundle -> Maybe Bundle
nextBundle (Bundle tl v) = fmap (Bundle tl) $ moveRight v

prevBundle :: Bundle -> Maybe Bundle
prevBundle (Bundle tl v) = fmap (Bundle tl) $ moveLeft v

siblingBundles :: Bundle -> Seq Bundle
siblingBundles (Bundle tl v) = fmap (Bundle tl) $ siblingViews v

-- | Adds serials for 'fwdGlobalSerial', 'revGlobalSerial',
-- 'fwdFileSerial', and 'revFileSerial'.  Any duplicate serials
-- already existing are overwritten.

addSerials
  :: Seq (Seq Transaction)
  -- ^ There is one input Seq for each file.  The inner Seq contains
  -- the transactions to which to assign serials.
  -> Seq (Seq Transaction)
addSerials = undefined


makeSerials
  :: (T.Traversable t1, T.Traversable t2, T.Traversable t3)
  => t1 (t2 (t3 a))
  -> t1 (t2 (t3 (a, Serial), Serial))
makeSerials = makeTxnSerials . makePstgSerials

makeFwd :: State Integer Tree
makeFwd = do
  this <- get
  modify succ
  return $ scalarChild "forward" this

makeRev :: State Integer Tree
makeRev = do
  modify pred
  this <- get
  return $ scalarChild "reverse" this

makeTxnSerials
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 a)
  -> t1 (t2 (a, Serial))
makeTxnSerials
  = fmap (fmap repack)
  . makeGlobalTxnSerials
  . fmap makeFileTxnSerials
  where
    repack ((a, FileSer f), GlblSer g) =
      (a, Serial $ treeChildren "serials" (Seq.fromList [g, f]))

makeGlobalTxnSerials
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 a)
  -> t1 (t2 (a, GlblSer))
makeGlobalTxnSerials sq = fst . flip runState 0 $
  (T.mapM (T.mapM assignToFwd) sq)
  >>= T.mapM (T.mapM (assignToRev "global" GlblSer))

makeFileTxnSerials
  :: T.Traversable t
  => t a
  -> t (a, FileSer)
makeFileTxnSerials sq = fst . flip runState 0 $
  (T.mapM assignToFwd sq)
  >>= T.mapM (assignToRev "file" FileSer)

newtype Serial = Serial Tree
  deriving (Eq, Ord, Show)

makePstgSerials
  :: (T.Traversable t1, T.Traversable t2, T.Traversable t3)
  => t1 (t2 (t3 a))
  -> t1 (t2 (t3 (a, Serial)))
makePstgSerials
  = fmap (fmap (fmap repack))
  . makeGlobalPstgSerials
  . fmap (makeFilePstgSerials)
  . fmap (fmap makeIndexSerials)
  where
    repack (((a, IndexSer i), FileSer f), GlblSer g)
      = (a, Serial $ treeChildren "serials" (Seq.fromList [g, f, i]))

newtype GlblSer = GlblSer Tree
  deriving (Eq, Ord, Show)

makeGlobalPstgSerials
  :: (T.Traversable t1, T.Traversable t2, T.Traversable t3)
  => t1 (t2 (t3 a))
  -> t1 (t2 (t3 (a, GlblSer)))
makeGlobalPstgSerials sq = fst . flip runState 0 $
  (T.mapM (T.mapM (T.mapM assignToFwd)) sq)
  >>= T.mapM (T.mapM (T.mapM (assignToRev "global" GlblSer)))

newtype FileSer = FileSer Tree
  deriving (Eq, Ord, Show)

makeFilePstgSerials
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 b)
  -> t1 (t2 (b, FileSer))
makeFilePstgSerials sq = fst . flip runState 0 $
  (T.mapM (T.mapM assignToFwd) sq)
  >>= T.mapM (T.mapM (assignToRev "file" FileSer))

newtype IndexSer = IndexSer Tree
  deriving (Eq, Ord, Show)

makeIndexSerials
  :: T.Traversable t
  => t a
  -> t (a, IndexSer)
makeIndexSerials bl = fst . flip runState 0 $
  (T.mapM assignToFwd bl)
  >>= T.mapM (assignToRev "index" IndexSer)

keepFst :: Monad m => m b -> a -> m (a, b)
keepFst m a = do { b <- m; return (a, b) }

assignToFwd :: a -> State Integer (a, Tree)
assignToFwd = keepFst makeFwd

assignToRev :: String -> (Tree -> b) -> (a, Tree) -> State Integer (a, b)
assignToRev lbl mkB (a, fwd) = do
  tree <- makeRev
  let tree' = treeChildren lbl (Seq.fromList [fwd, tree])
  return (a, mkB tree')

secondM :: Monad m => (a -> m b) -> (d, a) -> m (d, b)
secondM f (d, a) = do { b <- f a; return (d, b) }
