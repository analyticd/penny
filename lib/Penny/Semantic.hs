module Penny.Semantic where

import Data.Text (Text)

-- | Compares things that might have different representations that
-- have the same semantic meaning.  There are no formal properties for
-- this; the idea is that two items that \"mean the same thing\" will
-- return True.
class SemanticEq a where
  semanticEq :: a -> a -> Bool

class SemanticEq a => SemanticOrd a where
  semanticOrd :: a -> a -> Ordering

-- | Two 'Text' are semantically equal if they test equal using '=='.
instance SemanticEq Text where
  semanticEq = (==)

instance (SemanticEq a, SemanticEq b) => SemanticEq (Either a b) where
  semanticEq (Left x) (Left y) = semanticEq x y
  semanticEq (Right x) (Right y) = semanticEq x y
  semanticEq _ _ = False

