module Penny.NestedMap (
  NestedMap ( NestedMap ),
  unNestedMap,
  empty,
  relabel,
  modifyLabel,
  deepModifyLabel,
  deepRelabel,
  prune,
  cumulativeTotal,
  traverse,
  traverseWithTrail ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid ( Monoid, mconcat, mappend, mempty )
import Data.Maybe ( isJust )

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

-- | Inserts a new label in the top level of a NestedMap. Any existing
-- label at the given key is obliterated and replaced with the given
-- label. If the given key does not already exist, it is created. If
-- the given key already exists, then the existing NestedMaps nested
-- within are not changed. If the given key does not already exist, an
-- empty NestedMap becomes the children for the label.
relabel :: (Ord k) => NestedMap k l -> k -> l -> NestedMap k l
relabel m k l = modifyLabel m k (const l)

-- | Modifies a label in the top level of a NestedMap. The given
-- function is applied to Nothing if the key does not exist, or to
-- Just v if the key does exist. The given function returns the new
-- value for the label. If the given key already exists, then the
-- existing NestedMaps nested within that key are not changed. If the
-- given key does not already exist, an empty NestedMap becomes the
-- children for the label.
modifyLabel ::
  (Ord k)
  => NestedMap k l
  -> k
  -> (Maybe l -> l)
  -> NestedMap k l
modifyLabel (NestedMap m) k g = NestedMap n where
  n = M.alter f k m
  f Nothing = Just (g Nothing, NestedMap M.empty)
  f (Just (oldL, oldM)) = Just (g (Just oldL), oldM)

-- | Helper function for deepModifyLabel. For a given key and function
-- that modifies the label, return the new submap to insert into the
-- given map. Does not actually insert the submap though. That way,
-- deepModifyLabel can then modify the returned submap before
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
deepModifyLabel ::
  (Ord k)
  => NestedMap k l
  -> [(k, (Maybe l -> l))]
  -> NestedMap k l
deepModifyLabel m [] = m
deepModifyLabel (NestedMap m) ((k, f):vs) = let
  (newL, newM) = newSubmap (NestedMap m) k f
  newM' = deepModifyLabel newM vs
  in NestedMap $ M.insert k (newL, newM') m

-- | Similar to deepModifyLabel, but instead of granting the option of
-- modifying existing labels, the existing label is replaced with the
-- new label.
deepRelabel ::
  (Ord k)
  => NestedMap k l
  -> [(k, l)]
  -> NestedMap k l
deepRelabel m ls = deepModifyLabel m ls' where
  ls' = map (\(k, l) -> (k, const l)) ls

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
    f a@(l, m) = (totalTuple a, remapWithTotals m)

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
map1, map2, map3, map4 :: NestedMap Int String
map1 = NestedMap M.empty
map2 = deepRelabel map1 [(5, "hello"), (66, "goodbye"), (777, "yeah")]
map3 = deepRelabel map2 [(6, "what"), (77, "zeke"), (888, "foo")]
map4 = deepModifyLabel map3
       [ (6, (\m -> case m of Nothing -> "new"; (Just s) -> s ++ "new"))
       , (77, (\m -> case m of Nothing -> "new"; (Just s) -> s ++ "more new")) ]

printer :: Int -> String -> a -> IO (Maybe ())
printer i s _ = do
  putStrLn (show i)
  putStrLn s
  return $ Just ()

printerWithTrail :: [(Int, String)] -> Int -> String -> a -> IO (Maybe ())
printerWithTrail ps n str _ = do
  let printer (i, s) = putStr ("(" ++ show i ++ ", " ++ s ++ ") ")
  mapM_ printer . reverse $ ps
  printer (n, str)
  putStrLn ""
  return $ Just ()

showMap4 :: IO ()
showMap4 = do
  _ <- traverse printer map4
  return ()

showMapWithTrail :: IO ()
showMapWithTrail = do
  _ <- traverseWithTrail printerWithTrail map4
  return ()
