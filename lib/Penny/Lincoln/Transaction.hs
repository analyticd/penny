module Penny.Lincoln.Transaction where

import Control.Arrow (second)
import Penny.Lincoln.Ents
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Data.Sequence (Seq, viewl, ViewL(..))
import Data.Monoid
import qualified Data.Traversable as T
import Control.Monad.Trans.State

newtype TopLine = TopLine [Tree]
  deriving (Eq, Ord, Show)

data PstgMeta = PstgMeta [Tree] Trio
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
  => t1 (t2 (a, (t3 b)))
  -> t1 (t2 ((a, (t3 (b, Serial))), Serial))
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
  => t1 (t2 (a, b))
  -> t1 (t2 ((a, b), Serial))
makeTxnSerials = undefined
{-
  = fmap (fmap (second repack))
  . makeGlobalTxnSerials
  . fmap makeFileTxnSerials
  where
    repack ((a, FileSer f), GlblSer g) =
      (a, Serial $ treeChildren "serials" (Seq.fromList [g, f]))
-}
makeGlobalTxnSerials
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 (a, b))
  -> t1 (t2 ((a, b), GlblSer))
makeGlobalTxnSerials sq = fst . flip runState 0 $
  (T.mapM (T.mapM assignToFwd) sq)
  >>= T.mapM (T.mapM (assignToRev "global" GlblSer))

makeFileTxnSerials
  :: T.Traversable t
  => t (a, b)
  -> t (a, (b, FileSer))
makeFileTxnSerials = undefined
{-
makeFileTxnSerials sq = fst . flip runState 0 $
  (secondM (T.mapM assignToFwd) sq)
  >>= secondM (T.mapM (assignToRev "file" FileSer))
-}
newtype Serial = Serial Tree
  deriving (Eq, Ord, Show)

makePstgSerials
  :: (T.Traversable t1, T.Traversable t2, T.Traversable t3)
  => t1 (t2 (a, (t3 b)))
  -> t1 (t2 (a, (t3 (b, Serial))))
makePstgSerials
  = fmap (fmap (second (fmap repack)))
  . makeGlobalPstgSerials
  . fmap makeFilePstgSerials
  . fmap (fmap (second makeIndexSerials))
  where
    repack (((b, IndexSer i), FileSer f), GlblSer g) =
      (b, Serial $ treeChildren "serials" [g, f, i])


newtype GlblSer = GlblSer Tree
  deriving (Eq, Ord, Show)

makeGlobalPstgSerials
  :: (T.Traversable t1, T.Traversable t2, T.Traversable t3)
  => t1 (t2 (a, (t3 b)))
  -> t1 (t2 (a, (t3 (b, GlblSer))))
makeGlobalPstgSerials sqnce = fst . flip runState 0 $
  (mapGlobalPstg makeFwd sqnce)
  >>= mapGlobalPstgRev makeRev (wrapTree "global" GlblSer)

newtype FileSer = FileSer Tree
  deriving (Eq, Ord, Show)

makeFilePstgSerials
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (a, (t2 b))
  -> t1 (a, (t2 (b, FileSer)))
makeFilePstgSerials sq = fst . flip runState 0 $
  (T.mapM (secondM (T.mapM assignToFwd)) sq)
  >>= T.mapM (secondM (T.mapM (assignToRev "file" FileSer)))

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
  let tree' = treeChildren lbl [fwd, tree]
  return (a, mkB tree')

secondM :: Monad m => (a -> m b) -> (d, a) -> m (d, b)
secondM f (d, a) = do { b <- f a; return (d, b) }

mapGlobalPstg
  :: (Monad m, T.Traversable t1, T.Traversable t2, T.Traversable t3)
  => m c
  -> t1 (t2 (a, t3 b))
  -> m (t1 (t2 (a, t3 (b, c))))
mapGlobalPstg m = T.mapM (T.mapM f)
  where
    inner b = do
      c <- m
      return (b, c)
    f = secondM (T.mapM inner)

mapGlobalPstgRev
  :: (Monad m, T.Traversable t1, T.Traversable t2, T.Traversable t3)
  => m d
  -> (c -> d -> e)
  -> t1 (t2 (a, t3 (b, c)))
  -> m (t1 (t2 (a, t3 (b, e))))
mapGlobalPstgRev m comb = T.mapM (T.mapM f)
  where
    inner (b, c) = do
      d <- m
      return (b, comb c d)
    f = secondM (T.mapM inner)

wrapTree :: String -> (Tree -> a) -> Tree -> Tree -> a
wrapTree lbl wrp t1 t2 = wrp tre
  where
    tre = treeChildren lbl [t1, t2]
