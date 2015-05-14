module Penny.Semantic where

import Data.Text (Text)
import Data.Time (Day)

-- | Compares things that might have different representations that
-- have the same semantic meaning.  There are no formal properties for
-- this; the idea is that two items that \"mean the same thing\" will
-- return True.
class SemanticEq a where
  semanticEq :: a -> a -> Bool

class SemanticEq a => SemanticOrd a where
  semanticOrd :: a -> a -> Ordering

-- | Two 'Day' are semantically equal if they test equal using '=='.
instance SemanticEq Day where
  semanticEq = (==)

-- | Two 'Text' are semantically equal if they test equal using '=='.
instance SemanticEq Text where
  semanticEq = (==)

-- | Two 'Integer' are semantically equal if they are equal using
-- '=='.
instance SemanticEq Integer where
  semanticEq = (==)

instance SemanticOrd Integer where
  semanticOrd = compare

instance SemanticOrd Day where
  semanticOrd = compare

instance (SemanticEq a, SemanticEq b) => SemanticEq (Either a b) where
  semanticEq (Left x) (Left y) = semanticEq x y
  semanticEq (Right x) (Right y) = semanticEq x y
  semanticEq _ _ = False

