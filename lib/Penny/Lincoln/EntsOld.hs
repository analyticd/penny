-- | Balanced sets.  This module is the guardian of the core principle
-- of double-entry accounting, which is that all transactions must be
-- balanced.
module Penny.Lincoln.EntsOld where
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

import Penny.Lincoln.Balance
import Penny.Lincoln.Common
import Penny.Lincoln.Decimal
import Data.Maybe
import Control.Monad
import Data.List (find)
import qualified Data.Map as M
import Prelude hiding (exponent, negate)

-- | A written record of an entry, like what you would get from a
-- ledger file.  Holds the abstract (which represents both the
-- quantity and the side) and the arrangement.
data Record = Record
  { rAbstract :: Abstract Side
  , rArrange :: Arrangement
  } deriving (Eq, Ord, Show)

instance HasExponent Record where
  exponent = exponent . rAbstract

instance HasCoefficient Record where
  coefficient = coefficient . rAbstract

instance Signed Record where
  sign = sign . rAbstract

-- | Information from a single entry.  Always contains a 'Commodity'
-- and a 'Qty' which holds the quantity information in concrete form.
-- There is also a 'Maybe' 'Record', which is 'Just' only if the
-- 'ents' function was originally supplied with a 'Record'.  This
-- holds the quantity and commodity information as they were
-- originally written.
--
-- There is also arbitrary metadata.
data Ent a = Ent
  { entCommodity :: Commodity
  , entAbstract :: Maybe Record
  , entConcrete :: Qty
  , entMeta :: a
  } deriving (Eq, Ord, Show)

instance Functor Ent where
  fmap f e = e { entMeta = f (entMeta e) }

newtype Ents a = Ents { unEnts :: [Ent a] }
  deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f = Ents . map (fmap f) . unEnts

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
