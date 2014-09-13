module Typist.Constructor where

import qualified Typist.Typename as Ty

data T = T
  { name :: String
  , fields :: [Ty.T]
  } deriving (Eq, Ord, Show)

empty :: String -> T
empty s = T s []

-- | Produces a dot identifier.  Not quoted.
identify
  :: Ty.T
  -- ^ Name of type containing this ctor.
  -> String
  -- ^ Ctor name
  -> String
identify ty t = Ty.toString ty ++ " " ++ t

node
  :: String
  -- ^ Dot identifier
  -> String
  -- ^ Ctor name
  -> String
node idy n = quote idy ++ " [shape=box, label=" ++ n ++
  ", color=black" ++ "];\n"

edges
  :: String
  -- ^ Dot identifier
  -> [Ty.T]
  -- ^ Component types
  -> String
edges idy = unlines . map f
  where
    f ty = quote idy ++ " -> " ++ quote (Ty.toString ty) ++ ";"

quote :: String -> String
quote s = "\"" ++ s ++ "\""

