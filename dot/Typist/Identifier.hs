module Typist.Identifier where

import Data.List (intersperse)
import Data.List.Split (splitOn)

data T = T
  { modules :: [String]
  , name :: String
  } deriving (Eq, Ord, Show)

toString :: T -> String
toString (T ms n) = concat . intersperse "." $ ms ++ [n]

fromString :: String -> T
fromString s = case splitOn "." s of
  [] -> error "splitOn gave empty list"
  x:[] -> T [] x
  xs -> T (init xs) (last xs)
