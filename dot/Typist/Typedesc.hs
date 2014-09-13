module Typist.Typedesc where

import qualified Typist.Typename as Ty
import qualified Typist.Constructor as Ctor

data T = T
  { name :: Ty.T
  , ctors :: [Ctor.T Ty.T]
  } deriving (Eq, Ord, Show)

opaque :: Ty.T -> T
opaque n = T n []

dotifyCtor
  :: Ty.T
  -- ^ Name of type containing the ctor.
  -> Ctor.T Ty.T
  -> (String, String)
  -- ^ fst is the dottified text; snd is the name of the node for this
  -- ctor.
dotifyCtor cn t = (unlines $ line1 : map f (Ctor.fields t), nameThis)
  where
    labelThis = Ctor.name t
    line1 = nameThis ++ " [shape=box, label=" ++ labelThis ++ "];"
    f dest = nameThis ++ " -> " ++ quote (Ty.toString dest) ++ ";"
    nameThis = quote $ Ty.toString cn ++ " "
      ++ Ctor.name t

quote :: String -> String
quote s = "\"" ++ s ++ "\""

dotify :: T -> String
dotify t = unlines $ line1 : map f (ctors t)
  where
    nameThis = quote $ Ty.toString (name t)
    line1 = nameThis ++ ";"
    f ct =
       ctorTxt
       ++ nameThis ++ " -> "
       ++ ctorNodeName ++ " [style=dotted];"
      where
        (ctorTxt, ctorNodeName) = dotifyCtor (name t) ct

dotifyList :: [T] -> String
dotifyList ts =
  "digraph G {\n"
  ++ concatMap dotify ts
  ++ "}\n"

