module Typist.Typename where

import Data.List (intersperse)
import qualified Typist.Identifier as Identifier

data T = T
  { name :: Identifier.T
  , params :: [T]
  } deriving (Eq, Ord, Show)

toString :: T -> String
toString (T n ts) = case ts of
  [] -> ns
  _ -> ns ++ " " ++ (concat . intersperse " " . map f $ ts)
  where
    f c | null (params c) = toString c
        | otherwise = "(" ++ toString c ++ ")"
    ns = Identifier.toString n

noParams :: Identifier.T -> T
noParams s = T s []
