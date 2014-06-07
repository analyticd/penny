-- | Balanced sets.  This module is the guardian of the core principle
-- of double-entry accounting, which is that all transactions must be
-- balanced.
module Penny.Lincoln.Ents where

import Penny.Lincoln.Balance
import Penny.Lincoln.Common
import Penny.Lincoln.Decimal
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import Prelude hiding (exponent, negate)
import qualified Penny.Lincoln.Trio as T
import Data.Monoid

data Entrio
  = SQC NZGrouped Arrangement
  | SQ NZGrouped
  | SC
  | S
  | QC NZGrouped Arrangement
  | Q NZGrouped
  | C
  | N
  deriving (Eq, Ord, Show)

-- | Information from a single entry.  Always contains a 'Commodity'
-- and a 'Qty' which holds the quantity information in concrete form.
-- There is also a 'Maybe' 'Record', which is 'Just' only if the
-- 'ents' function was originally supplied with a 'Record'.  This
-- holds the quantity and commodity information as they were
-- originally written.
--
-- There is also arbitrary metadata.
data Ent a = Ent
  { entConcrete :: Qty
  , entCommodity :: Commodity
  , entTrio :: Entrio
  , entMeta :: a
  } deriving (Eq, Ord, Show)

instance Functor Ent where
  fmap f e = e { entMeta = f (entMeta e) }

newtype Ents a = Ents { unEnts :: [Ent a] }
  deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f = Ents . map (fmap f) . unEnts


data ErrorCode
  = SQNoCommodities
  | SQMultipleCommodities
  | SCNoCommodities
  | SCWrongCommodity
  | SCWrongSide
  | SCMultipleCommodities
  | SNoCommodities
  | SWrongCommodity
  | SWrongSide
  | CommodityNotFound
  | NoCommoditiesInBalance
  | MultipleCommoditiesInBalance
  | QQtyTooBig
  deriving (Eq, Ord, Show)

data Error = Error
  { errCode :: ErrorCode
  , errTrio :: T.Trio
  , errBalances :: Balances
  } deriving (Eq, Ord, Show)

buildEntrio :: T.Trio -> Entrio
buildEntrio t = case t of
  T.SQC _ nzg _ ar -> SQC nzg ar
  T.SQ _ nzg -> SQ nzg
  T.SC _ _ -> SC
  T.S _ -> S
  T.QC nzg _ ar -> QC nzg ar
  T.Q nzg -> Q nzg
  T.C _ -> C
  T.N -> N

procTrio
  :: Balances
  -> T.Trio
  -> Either Error (Balances, Qty, Commodity)
procTrio bal trio = case trio of

  T.SQC s nzg cy _ -> Right (bal', q, cy)
    where
      bal' = bal <> balance cy q
      prms = Params (sign s) (coefficient nzg) (exponent nzg)
      q = Qty $ normal prms

  T.SQ s nzg -> case singleCommodity of
    Left e -> Left e
    Right (cy, _, _) -> Right (bal', q, cy)
      where
        bal' = bal <> balance cy q
        prms = Params (sign s) (coefficient nzg) (exponent nzg)
        q = Qty $ normal prms

  T.SC s cy -> case lookupCommodity cy of
    Left e -> Left e
    Right (sBal, q)
      | sBal /= opposite s -> Left $ Error SCWrongSide trio bal
      | otherwise -> Right (bal', q', cy)
      where
        bal' = bal <> balance cy q'
        q' = Qty . negate . unQty $ q

  T.S s -> case singleCommodity of
    Left e -> Left e
    Right (cy, sBal, q)
      | sBal /= opposite s -> Left $ Error SWrongSide trio bal
      | otherwise -> Right (bal', q', cy)
      where
        bal' = bal <> balance cy q'
        q' = Qty . negate . unQty $ q

  T.QC nzg cy _ -> case lookupCommodity cy of
    Left e -> Left e
    Right (s, _) -> Right (bal', q, cy)
      where
        q = Qty $ normal pms
        pms = Params (sign . opposite $ s) (coefficient nzg)
                (exponent nzg)
        bal' = bal <> balance cy q

  T.Q nzg -> case singleCommodity of
    Left e -> Left e
    Right (cy, s, balQ)
      | abs (unQty q') > abs (unQty balQ) -> Left $ Error
          QQtyTooBig trio bal
      | otherwise -> Right (bal', q', cy)
      where
        q' = Qty . normal $ Params (sign . opposite $ s)
          (coefficient nzg) (exponent nzg)
        bal' = bal <> balance cy q'

  T.C cy -> case lookupCommodity cy of
    Left e -> Left e
    Right (_, balQ) -> Right (bal', q', cy)
      where
        q' = Qty . negate . unQty $ balQ
        bal' = bal <> balance cy q'

  T.N -> case singleCommodity of
    Left e -> Left e
    Right (cy, _, balQ) -> Right (bal', q', cy)
      where
        q' = Qty . negate . unQty $ balQ
        bal' = bal <> balance cy q'

  where

    -- Looks up a commodity in the given 'Balances'.  First, removes all
    -- balanced commodities from the 'Balances'.  Then finds the requested
    -- commodity and returns its balance.  Fails if the requested
    -- commodity is not present.

    lookupCommodity :: Commodity -> Either Error (Side, Qty)
    lookupCommodity cy = case M.lookup cy . onlyUnbalanced $ bal of
      Nothing -> Left $ Error CommodityNotFound trio bal
      Just (s, q) -> return (s, q)

    -- Gets a single commodity from the given 'Balances', if it has just
    -- a single commodity.

    singleCommodity :: Either Error (Commodity, Side, Qty)
    singleCommodity = case M.assocs . onlyUnbalanced $ bal of
      [] -> Left $ Error NoCommoditiesInBalance trio bal
      (cy, (s, q)):[] -> Right (cy, s, q)
      _ -> Left $ Error MultipleCommoditiesInBalance trio bal

{-
  T.SC s cy -> case M.assocs . onlyUnbalanced $ bal of
    [] -> Left $ Error SCNoCommodities trio bal
    (balCy, (balS, balQ)):[]
      | balCy /= cy -> Left $ Error SCWrongCommodity trio bal
      | balS /= opposite s -> Left $ Error SCWrongSide trio bal
      | otherwise -> Right (bal', Ent q cy etro mta)
      where
        bal' = bal <> balance cy q
        q = Qty . negate . unQty $ balQ
        etro = SC
    _ -> Left $ Error SCMultipleCommodities trio bal
-}

{-
  ( Record(..)
  , Ent
  , entCommodity
  , entAbstract
  , entConcrete
  , entMeta
  , Ents
  , unEnts
  , ents
  , rearrange
  ) where

rearrange :: Arrangement -> Ent a -> Ent a
rearrange a (Ent c r q m) = Ent c r' q m
  where
    r' = case r of
      Nothing -> Nothing
      Just (Record ab _) -> Just (Record ab a)

-- | Creates an 'Ents' only if the data is balanced.
ents
  :: [(Commodity, Maybe (Either Record Qty), a)]
  -> Maybe (Ents a)
ents ls = do
  let conc = map concretize ls
      bals = balances . map (\(c, _, q, a) -> (c, q, a)) $ conc
  inf <- inferable bals . map (\(c, _, q, _) -> (c, q)) $ conc
  es <- mkEnts inf conc
  return $ Ents es

mkEnts
  :: Maybe Qty
  -> [(Commodity, Maybe Record, Maybe Qty, a)]
  -> Maybe [Ent a]
mkEnts mq = mapM mkE
  where
    mkE (c, mayR, mayQ, a) = case mayQ of
      Just q -> Just $ Ent c mayR q a
      Nothing -> case mq of
        Nothing -> Nothing
        Just q -> Just $ Ent c mayR q a


concretize
  :: (Commodity, Maybe (Either Record Qty), a)
  -> (Commodity, Maybe Record, Maybe Qty, a)
concretize (cy, mayEi, a) = (cy, mayRec, mayQty, a)
  where
    (mayRec, mayQty) = case mayEi of
      Nothing -> (Nothing, Nothing)
      Just ei -> case ei of
        Left rc -> (Just rc, Just . Qty . normal . rAbstract $ rc)
        Right q -> (Nothing, Just q)


balances :: [(Commodity, Maybe Qty, a)] -> Balances
balances = foldl adder emptyBalances
  where
    adder bal (c, mq, _) = case mq of
      Nothing -> bal
      Just q -> addEntry c q bal

-- | Finds the single inferable quantity, if there is one.  Fails if
-- the list is not balanced and there is no single inferable quantity.
-- If the list is balanced, returns Just Nothing.  If the list is not
-- balanced but has a single inferable entry, returns the new quantity.

inferable :: Balances -> [(Commodity, Maybe Qty)] -> Maybe (Maybe Qty)
inferable bals ls =
  let mayCyToInfer = fmap fst $ find (isNothing . snd) ls
  in case mayCyToInfer of
      Nothing -> if needsNoInference bals
        then return Nothing
        else mzero
      Just cy -> case inferQty cy bals of
        Nothing -> mzero
        Just q -> return (Just q)

-- | When passed a commodity to infer, returns the inferred quantity.
-- Fails if:
--
-- * The 'Balances' is already balanced
--
-- * The 'Balances' has more than one commodity that is not balanced
--
-- * The 'Balances' has one commodity to infer, but it does not match
-- the commodity passed in.

inferQty :: Commodity -> Balances -> Maybe Qty
inferQty c b = case M.assocs . unBalances . onlyUnbalanced $ b of
  [] -> Nothing
  (c', q):[]
    | c' /= c -> Nothing
    | otherwise -> Just . Qty . negate . unQty $ q
  _ -> Nothing

-- | Make sure that a Balances needs no inference.
needsNoInference :: Balances -> Bool
needsNoInference = M.null . unBalances . onlyUnbalanced

-}
