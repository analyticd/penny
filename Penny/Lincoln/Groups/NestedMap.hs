module Penny.NestedMap (
  NestedMap ( NestedMap ),
  unNestedMap,
  empty,
  relabel,
  cumulativeTotal,
  traverse,
  traverseWithTrail ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid ( Monoid, mconcat, mappend, mempty )

data NestedMap k l =
  NestedMap { unNestedMap :: Map k (l, NestedMap k l) }
  deriving Show

instance Functor (NestedMap k) where
  fmap f (NestedMap m) = let
    g (l, s) = (f l, fmap f s)
    in NestedMap $ M.map g m

-- | An empty NestedMap.
empty :: NestedMap k l
empty = NestedMap (M.empty)

-- | Helper function for relabel. For a given key and function
-- that modifies the label, return the new submap to insert into the
-- given map. Does not actually insert the submap though. That way,
-- relabel can then modify the returned submap before
-- inserting it into the mother map with the given label.
newSubmap ::
  (Ord k)
  => NestedMap k l
  -> k
  -> (Maybe l -> l)
  -> (l, NestedMap k l)
newSubmap (NestedMap m) k g = (newL, NestedMap newM) where
  (newL, newM) = case M.lookup k m of
    Nothing -> (g Nothing, M.empty)
    (Just (oldL, (NestedMap oldM))) -> (g (Just oldL), oldM)

-- | Descends through a NestedMap with successive keys in the list. At
-- any given level, if the key given does not already exist, then
-- inserts an empty submap and applies the given label modification
-- function to Nothing to determine the new label. If the given key
-- already does exist, then preserves the existing submap and applies
-- the given label modification function to (Just oldlabel) to
-- determine the new label.
relabel ::
  (Ord k)
  => NestedMap k l
  -> [(k, (Maybe l -> l))]
  -> NestedMap k l
relabel m [] = m
relabel (NestedMap m) ((k, f):vs) = let
  (newL, newM) = newSubmap (NestedMap m) k f
  newM' = relabel newM vs
  in NestedMap $ M.insert k (newL, newM') m

totalMap ::
  (Monoid l)
  => NestedMap k l
  -> l
totalMap (NestedMap m) =
  if M.null m
  then mempty
  else mconcat . map totalTuple . M.elems $ m

totalTuple ::
  (Monoid l)
  => (l, NestedMap k l)
  -> l
totalTuple (l, (NestedMap top)) =
  if M.null top
  then l
  else mappend l (totalMap (NestedMap top))

remapWithTotals ::
  (Monoid l)
  => NestedMap k l
  -> NestedMap k l
remapWithTotals (NestedMap top) =
  if M.null top
  then NestedMap M.empty
  else NestedMap $ M.map f top where
    f a@(_, m) = (totalTuple a, remapWithTotals m)

-- | Leaves all keys of the map and submaps the same. Changes each
-- label to reflect the total of that label and of all the submaps
-- accompanying the label.
cumulativeTotal ::
  (Monoid l)
  => NestedMap k l
  -> (l, NestedMap k l)
cumulativeTotal m = (totalMap m, remapWithTotals m)

traverse ::
  (Monad m, Ord k)
  => (k -> l -> NestedMap k l -> m (Maybe a))
  -> NestedMap k l
  -> m (NestedMap k a)
traverse f m = traverseWithTrail (\_ -> f) m

traverseWithTrail ::
  (Monad m, Ord k)
  => ( [(k, l)] -> k -> l -> NestedMap k l -> m (Maybe a) )
  -> NestedMap k l
  -> m (NestedMap k a)
traverseWithTrail f = traverseWithTrail' f []

traverseWithTrail' ::
  (Monad m, Ord k)
  => ([(k, l)] -> k -> l -> NestedMap k l -> m (Maybe a))
  -> [(k, l)]
  -> NestedMap k l
  -> m (NestedMap k a)
traverseWithTrail' f ts (NestedMap m) =
  if M.null m
  then return $ NestedMap M.empty
  else do
    let ps = M.assocs m
    mlsMaybes <- mapM (traversePairWithTrail f ts) ps
    let ps' = zip (M.keys m) mlsMaybes
        folder (k, ma) rs = case ma of
          (Just r) -> (k, r):rs
          Nothing -> rs
        ps'' = foldr folder [] ps'
    return (NestedMap (M.fromList ps''))

traversePairWithTrail ::
  (Monad m, Ord k)
  => ( [(k, l)] -> k -> l -> NestedMap k l -> m (Maybe a) )
  -> [(k, l)]
  -> (k, (l, NestedMap k l))
  -> m (Maybe (a, NestedMap k a))
traversePairWithTrail f ls (k, (l, m)) = do
  ma <- f ls k l m
  case ma of
    Nothing -> return Nothing
    (Just a) -> do
      m' <- traverseWithTrail' f ((k, l):ls) m
      return (Just (a, m'))

-- For testing
_new :: (k, l) -> (k, (Maybe l -> l))
_new (k, l) = (k, const l)

_map1, _map2, _map3, _map4 :: NestedMap Int String
_map1 = NestedMap M.empty
_map2 = relabel _map1 [_new (5, "hello"), _new (66, "goodbye"), _new (777, "yeah")]
_map3 = relabel _map2 [_new (6, "what"), _new (77, "zeke"), _new (888, "foo")]
_map4 = relabel _map3
       [ (6, (\m -> case m of Nothing -> "_new"; (Just s) -> s ++ "_new"))
       , (77, (\m -> case m of Nothing -> "_new"; (Just s) -> s ++ "more _new")) ]

_printer :: Int -> String -> a -> IO (Maybe ())
_printer i s _ = do
  putStrLn (show i)
  putStrLn s
  return $ Just ()

_printerWithTrail :: [(Int, String)] -> Int -> String -> a -> IO (Maybe ())
_printerWithTrail ps n str _ = do
  let ptr (i, s) = putStr ("(" ++ show i ++ ", " ++ s ++ ") ")
  mapM_ ptr . reverse $ ps
  ptr (n, str)
  putStrLn ""
  return $ Just ()

_showMap4 :: IO ()
_showMap4 = do
  _ <- traverse _printer _map4
  return ()

_showMapWithTrail :: IO ()
_showMapWithTrail = do
  _ <- traverseWithTrail _printerWithTrail _map4
  return ()
