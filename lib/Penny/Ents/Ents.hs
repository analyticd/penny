{-# LANGUAGE ScopedTypeVariables #-}
module Penny.Ents.Ents where

import Penny.Common
import Penny.Numbers.Qty
import Data.Monoid
import qualified Penny.Ents.Trio as T
import qualified Data.Map as M
import Penny.Numbers.Concrete
import Prelude hiding (negate)

data Entrio a b
  = QC a Arrangement
  | Q a
  | SC
  | S
  | UC b Arrangement
  | U b
  | C
  | E
  deriving (Eq, Ord, Show)

data Ent a b m = Ent
  { entQty :: Qty
  , entCommodity :: Commodity
  , entTrio :: Entrio a b
  , entMeta :: m
  } deriving (Eq, Ord, Show)

instance Functor (Ent a b) where
  fmap f e = e { entMeta = f (entMeta e) }

newtype Ents a b m = Ents { unEnts :: [Ent a b m] }
  deriving (Eq, Ord, Show)

instance Functor (Ents a b) where
  fmap f = Ents . map (fmap f) . unEnts

instance Monoid (Ents a b m) where
  mempty = Ents []
  mappend (Ents x) (Ents y) = Ents $ x ++ y

entToTrio :: Ent a b m -> T.Trio a b
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
rearrange :: Arrangement -> Ent a b m -> Ent a b m
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
data EntError a b = EntError
  { errCode :: ErrorCode
  -- ^ The exact nature of the error.

  , errTrio :: T.Trio a b
  -- ^ The 'T.Trio' that caused the error.

  , errBalances :: M.Map Commodity (Side, Qty)
  -- ^ The balances that existed at the time the error occurred.
  } deriving (Eq, Ord, Show)

-- | An error arose while attempting to create an 'Ents'.  The error
-- may have arose while processing an individual 'T.Trio' to an 'Ent'.
-- Or, processing of all 'Ent's may have succeeded, but if the total
-- of all the postings is not balanced, an error occurs.
newtype Error a b
  = Error { unError :: Either (EntError a b) UnbalancedAtEnd }
  deriving (Eq, Ord, Show)

-- | The total of all 'Ent' is not balanced.
data UnbalancedAtEnd = UnbalancedAtEnd
  { uaeBalances :: M.Map Commodity (Side, Qty) }
  deriving (Eq, Ord, Show)


{-
procEnt
  :: Balances
  -> (T.Trio, a)
  -> Either EntError (Balances, Ent a)
procEnt bals (tri, mta) = fmap f $ procTrio unbals tri
  where
    unbals = onlyUnbalanced bals
    f (q, cy) = (bals', ent)
      where
        ent = Ent q cy (buildEntrio tri) mta
        bals' = bals <> balance cy q
-}


buildEntrio :: T.Trio a b -> Entrio a b
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
  :: forall a b
  .  (a -> Qty)
  -> (b -> Side -> Qty)
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
    Right (s, _) -> Right (fb b (opposite s), cy)

  T.U b -> case singleCommodity of
    Left e -> Left e
    Right (cy, s, balQ)
      | abs (unQty q') > abs (unQty balQ) -> Left $ EntError
          QQtyTooBig trio bal
      | otherwise -> Right (q', cy)
      where
        q' = fb b (opposite s)

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

    lookupCommodity :: Commodity -> Either (EntError a b) (Side, Qty)
    lookupCommodity cy = case M.lookup cy bal of
      Nothing -> Left $ EntError CommodityNotFound trio bal
      Just (s, q) -> return (s, q)

    -- Gets a single commodity from the given 'Balances', if it has just
    -- a single commodity.

    singleCommodity :: Either (EntError a b) (Commodity, Side, Qty)
    singleCommodity = case M.assocs bal of
      [] -> Left $ EntError NoCommoditiesInBalance trio bal
      (cy, (s, q)):[] -> Right (cy, s, q)
      _ -> Left $ EntError MultipleCommoditiesInBalance trio bal


