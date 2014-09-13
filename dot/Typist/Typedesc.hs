module Typist.Typedesc where

import qualified Typist.Typename as Ty
import qualified Typist.Constructor as Ctor
import Data.List (intersperse)

data T = T
  { name :: String
  , params :: [Ty.T]
  , ctors :: [Ctor.T Ty.T]
  } deriving (Eq, Ord, Show)

nullary
  :: String
  -- ^ Type name
  -> [(String, [Ty.T])]
  -- ^ Each constructor and its fields
  -> T
nullary n cs = T n [] (map (uncurry Ctor.T) cs)

dotifyCtor :: Ctor.T Ty.T -> String
dotifyCtor t = unlines $ line1 : map f (Ctor.fields t)
  where
    nameThis = quote . Ctor.name $ t
    line1 = nameThis ++ " [shape=box];"
    f dest = nameThis ++ " -> " ++ quote (Ty.toString dest) ++ ";"

quote :: String -> String
quote s = "\"" ++ s ++ "\""

dotify :: T -> String
dotify t = unlines $ line1 : map f (ctors t)
  where
    nameThis = typeNode (name t) (params t)
    line1 = nameThis ++ ";"
    f (Ctor.T cName cFlds) = nameThis ++ " -> "
       ++ typeNode cName cFlds ++ " [style=dotted];"

dotifyList :: [T] -> String
dotifyList ts =
  "digraph G {\n"
  ++ concatMap dotify ts
  ++ "}\n"

typeNode
  :: String
  -- ^ Type name
  -> [Ty.T]
  -- ^ Type parameters
  -> String
  -- ^ Quoted node name
typeNode n ts = quote $ n ++ rest
  where
    rest = case ts of
      [] -> ""
      _ -> " " ++ (concat . intersperse " " . map Ty.toString $ ts)
