module Typist.Typename where

import Data.List (intersperse)

data T = T
  { name :: String
  , params :: [T]
  } deriving (Eq, Ord, Show)

toString :: T -> String
toString (T n ts) = case ts of
  [] -> n
  _ -> n ++ " " ++ (concat . intersperse " " . map f $ ts)
  where
    f c | null (params c) = name c
        | otherwise = "(" ++ toString c ++ ")"

noParams :: String -> T
noParams s = T s []
