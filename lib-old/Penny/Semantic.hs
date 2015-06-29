module Penny.Semantic where

import Data.Text (Text)
import Data.Time (Day)

-- | Compares things that might have different representations that
-- have the same semantic meaning.  There are no formal properties for
-- this; the idea is that two items that \"mean the same thing\" will
-- return True.
class SemanticEq a where
  (==@) :: a -> a -> Bool
  x ==@ y = not $ x /=@ y

  (/=@) :: a -> a -> Bool
  x /=@ y = not $ x ==@ y

infix 4 ==@
infix 4 /=@

class SemanticEq a => SemanticOrd a where
  compareSemantic :: a -> a -> Ordering
  (<@) :: a -> a -> Bool
  (>=@) :: a -> a -> Bool
  (>@) :: a -> a -> Bool
  (<=@) :: a -> a -> Bool
  maxSemantic :: a -> a -> a
  minSemantic :: a -> a -> a

  compareSemantic x y
    | x ==@ y = EQ
    | x <=@ y = LT
    | otherwise = GT

  x <@ y = case compareSemantic x y of { LT -> True; _ -> False }
  x >=@ y = case compareSemantic x y of { LT -> False; _ -> True }
  x >@ y = case compareSemantic x y of { GT -> True; _ -> False }
  x <=@ y = case compareSemantic x y of { GT -> False; _ -> True }
  maxSemantic x y = if x <=@ y then y else x
  minSemantic x y = if x <=@ y then x else y

-- | Two 'Day' are semantically equal if they test equal using '=='.
instance SemanticEq Day where
  (==@) = (==)

-- | Two 'Text' are semantically equal if they test equal using '=='.
instance SemanticEq Text where
  (==@) = (==)

-- | Two 'Integer' are semantically equal if they are equal using
-- '=='.
instance SemanticEq Integer where
  (==@) = (==)

instance SemanticOrd Integer where
  compareSemantic = compare

instance SemanticOrd Day where
  compareSemantic = compare

instance (SemanticEq a, SemanticEq b) => SemanticEq (Either a b) where
  (Left x) ==@ (Left y) = x ==@ y
  (Right x) ==@ (Right y) = x ==@ y
  _ ==@ _ = False

newtype Semantic a = Semantic a deriving Show

instance SemanticEq a => Eq (Semantic a) where
  Semantic x == Semantic y = x ==@ y

instance SemanticOrd a => Ord (Semantic a) where
  compare (Semantic x) (Semantic y) = compareSemantic x y
