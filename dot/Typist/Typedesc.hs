module Typist.Typedesc where

import qualified Typist.Typename as Ty
import qualified Typist.Constructor as Ctor

data T = T
  { name :: Ty.T
  , ctors :: [Ctor.T]
  } deriving (Eq, Ord, Show)

abstract :: Ty.T -> T
abstract n = T n []



quote :: String -> String
quote s = "\"" ++ s ++ "\""

dotify :: T -> String
dotify t = concat $ line1 : map f (ctors t)
  where
    nameThis = quote $ Ty.toString (name t)
    line1 = nameThis ++ " [color=black];\n"
    f ct = ctorNode ++ ctorEdges ++ edgeToCtor
      where
        ctorId = Ctor.identify (name t) (Ctor.name ct)
        ctorNode = Ctor.node ctorId (Ctor.name ct)
        ctorEdges = Ctor.edges ctorId (Ctor.fields ct)
        edgeToCtor = nameThis ++ " -> " ++ quote ctorId ++
          " [style=dotted];\n"

dotifyList :: [T] -> String
dotifyList ts =
  "digraph G {\n"
  ++ "node [color=red];\n"
  ++ concatMap dotify ts
  ++ "}\n"

