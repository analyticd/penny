module Penny.Ents.Ents where

import Penny.Balance
import Penny.Common
import Penny.Numbers.Qty
import Data.Monoid
import qualified Penny.Ents.Trio as T
import qualified Data.Map as M
import Penny.Numbers.Concrete
import qualified Deka.Native.Abstract as DN
import Prelude hiding (negate)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup

data Entrio
  = QC (Either (Abstract Period Side) (Abstract Comma Side)) Arrangement
  | Q (Either (Abstract Period Side) (Abstract Comma Side))
  | SC
  | S
  | UC (Either (Unpolar Period) (Unpolar Comma)) Arrangement
  | U (Either (Unpolar Period) (Unpolar Comma))
  | C
  | E
  deriving (Eq, Ord, Show)

data Ent m = Ent
  { entQty :: Qty
  , entCommodity :: Commodity
  , entTrio :: Entrio
  , entMeta :: m
  } deriving (Eq, Ord, Show)

instance Functor Ent where
  fmap f e = e { entMeta = f (entMeta e) }

newtype Ents m = Ents { unEnts :: [Ent m] }
  deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f = Ents . map (fmap f) . unEnts

instance Monoid (Ents m) where
  mempty = Ents []
  mappend (Ents x) (Ents y) = Ents $ x ++ y

entToTrio :: Ent m -> T.Trio
entToTrio (Ent q c t _) = case t of
  QC a ar -> T.QC a c ar
  Q a -> T.Q a
  SC -> T.SC s c
  S -> T.S s
  UC b ar -> T.UC b c ar
  U b -> T.U b
  C -> T.C c
  E -> T.E
  where
    s = case qtySide q of
      Nothing -> error "entToTrio: qty has no side"
      Just sd -> sd

-- | Change the 'Arrangement' in an 'Ent'.  Does nothing if the 'Ent'
-- has no 'Arrangement' to begin with.
rearrange :: Arrangement -> Ent m -> Ent m
rearrange a' e = e { entTrio = e' }
  where
    e' = case entTrio e of
      QC a _ -> QC a a'
      UC b _ -> UC b a'
      x -> x

-- | Different errors that may arise when processing a single 'T.Trio'
-- for conversion to an 'Ent'.
data ErrorCode
  = SCWrongSide
  | SWrongSide
  | CommodityNotFound
  | NoCommoditiesInBalance
  | MultipleCommoditiesInBalance
  | QQtyTooBig
  deriving (Eq, Ord, Show)

-- | An error occurred while attempting to create an 'Ent'.
data EntError = EntError
  { errCode :: ErrorCode
  -- ^ The exact nature of the error.

  , errTrio :: T.Trio
  -- ^ The 'T.Trio' that caused the error.

  , errBalances :: M.Map Commodity (Side, Qty)
  -- ^ The balances that existed at the time the error occurred.
  } deriving (Eq, Ord, Show)

-- | An error arose while attempting to create an 'Ents'.  The error
-- may have arose while processing an individual 'T.Trio' to an 'Ent'.
-- Or, processing of all 'Ent's may have succeeded, but if the total
-- of all the postings is not balanced, an error occurs.
newtype Error
  = Error { unError :: Either EntError UnbalancedAtEnd }
  deriving (Eq, Ord, Show)

-- | The total of all 'Ent' is not balanced.
data UnbalancedAtEnd = UnbalancedAtEnd
  { uaeBalances :: M.Map Commodity (Side, Qty) }
  deriving (Eq, Ord, Show)


procEnt
  :: Balances
  -> (T.Trio, m)
  -> Either EntError (Balances, Ent m)
procEnt bals (tri, mta) = fmap f $ procTrio unbals tri
  where
    unbals = onlyUnbalanced bals
    f (q, cy) = (bals', ent)
      where
        ent = Ent q cy (buildEntrio tri) mta
        bals' = bals <> balance cy q


procEntM
  :: (T.Trio, m)
  -> EitherT EntError (State Balances) (Ent m)
procEntM (tri, mta) = do
  bal <- lift get
  case procEnt bal (tri, mta) of
    Left e -> left e
    Right (bal', r) -> do
      lift $ put bal'
      return r


-- | Only creates an 'Ents' if all the 'T.Trio' are balanced.  See
-- 'T.Trio' for more information on the rules this function follows.
ents
  :: [(T.Trio a b, m)]
  -> Either (Error a b) (Ents a b m)
ents ls =
  let (finalEi, finalBal) = flip runState emptyBalances
       . runEitherT . mapM procEntM $ ls
  in case finalEi of
      Left e -> Left . Error . Left $ e
      Right es
        | M.null unbals -> Right (Ents es)
        | otherwise -> Left . Error . Right . UnbalancedAtEnd $ unbals
        where
          unbals = onlyUnbalanced finalBal


buildEntrio :: T.Trio -> Entrio
buildEntrio t = case t of
  T.QC a _ ar -> QC a ar
  T.Q a -> Q a
  T.SC _ _ -> SC
  T.S _ -> S
  T.UC b _ ar -> UC b ar
  T.U b -> U b
  T.C _ -> C
  T.E -> E

procTrio
  :: (a -> Qty)
  -> (b -> (DN.Coefficient, Exponent))
  -> M.Map Commodity (Side, Qty)
  -> T.Trio a b
  -> Either (EntError a b) (Qty, Commodity)

procTrio fa fb bal trio = case trio of

  T.QC a cy _ -> Right (fa a, cy)

  T.Q a -> case singleCommodity of
    Left e -> Left e
    Right (cy, _, _) -> Right (fa a, cy)

  T.SC s cy -> case lookupCommodity cy of
    Left e -> Left e
    Right (sBal, q)
      | sBal /= opposite s -> Left $ EntError SCWrongSide trio bal
      | otherwise -> Right (q', cy)
      where
        q' = Qty . negate . unQty $ q

  T.S s -> case singleCommodity of
    Left e -> Left e
    Right (cy, sBal, q)
      | sBal /= opposite s -> Left $ EntError SWrongSide trio bal
      | otherwise -> Right (q', cy)
      where
        q' = Qty . negate . unQty $ q

  T.UC b cy _ -> case lookupCommodity cy of
    Left e -> Left e
    Right (s, _) -> Right (q, cy)
      where
        (c, e) = fb b
        q = Qty . concrete $ Params (sideToSign (opposite s)) c e

  T.U b -> case singleCommodity of
    Left e -> Left e
    Right (cy, s, balQ)
      | abs (unQty q') > abs (unQty balQ) -> Left $ EntError
          QQtyTooBig trio bal
      | otherwise -> Right (q', cy)
      where
        (c, e) = fb b
        q' = Qty . concrete $ Params (sideToSign (opposite s)) c e

  T.C cy -> case lookupCommodity cy of
    Left e -> Left e
    Right (_, balQ) -> Right (q', cy)
      where
        q' = Qty . negate . unQty $ balQ

  T.E -> case singleCommodity of
    Left e -> Left e
    Right (cy, _, balQ) -> Right (q', cy)
      where
        q' = Qty . negate . unQty $ balQ

  where

    -- Looks up a commodity in the given 'Balances'.  Then finds the
    -- requested commodity and returns its balance.  Fails if the
    -- requested commodity is not present.

    -- lookupCommodity :: Commodity -> Either (EntError a b) (Side, Qty)
    lookupCommodity cy = case M.lookup cy bal of
      Nothing -> Left $ EntError CommodityNotFound trio bal
      Just (s, q) -> return (s, q)

    -- Gets a single commodity from the given 'Balances', if it has just
    -- a single commodity.

    -- singleCommodity :: Either (EntError a b) (Commodity, Side, Qty)
    singleCommodity = case M.assocs bal of
      [] -> Left $ EntError NoCommoditiesInBalance trio bal
      (cy, (s, q)):[] -> Right (cy, s, q)
      _ -> Left $ EntError MultipleCommoditiesInBalance trio bal


{-
-- | Creates 'Ents' but, unlike 'ents', never fails.  To make this
-- guarantee, 'rEnts' puts restrictions on its arguments.

rEnts
  :: Side
  -- ^ All postings that you supply are on this 'Side'

  -> Commodity
  -- ^ All postings will have this same 'Commodity'

  -> Arrangement
  -- ^ All postings except for the inferred one will use this
  -- 'Arrangement'

  -> m
  -- ^ Metadata for inferred posting

  -> [(DN.Coefficient, Exponent, 

  -> [(u, m)]
  -- ^ Each unsigned number, and its corresponding metadata

  -> (u -> (DN.Coefficient, Exponent))
  -- ^ How to get the unsinged information from the unsigned number

  -> Ents a u m
  -- ^ The resulting 'Ents'.  If the list of 'NZGrouped' was
  -- non-empty, then a single inferred posting is at the end to
  -- balance out the other non-zero postings.  Each 'Ent' has an
  -- 'Entrio' of 'SZC', except for the inferred posting, which is 'E'.
-}
rEnts = undefined
{-

rEnts s cy ar mt ls
  | null ls = Ents []
  | otherwise = Ents $ zipWith mkEnt qs ls ++ [inferred]
  where
    qs = map mkQ . map fst $ ls
    mkQ nzg = Qty . normal $ Params (sign s) (coefficient nzg)
      (exponent nzg)
    offset = Qty . negate . sum . map unQty $ qs
    inferred = Ent offset cy E mt
    mkEnt q (nzg, m) = Ent q cy (SZC nzg ar) m
-}
