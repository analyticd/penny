module Main where

import qualified Data.Char as C

singleChars = [
  ("exclamation", "!")
  , ("quote", "\"")
  , ("hash", "#")
  , ("dollar", "$")
  , ("percent", "%")
  , ("ampersand", "&")
  , ("apostrophe", "'")
  , ("openParen", "(")
  , ("closeParen", ")")
  , ("asterisk", "*")
  , ("plus", "+")
  , ("comma", ",")
  , ("dash", "-")
  , ("period", ".")
  , ("slash", "/")
  , ("colon", ":")
  , ("semicolon", ";")
  , ("lessThan", "<")
  , ("greaterThan", ">")
  , ("question", "?")
  , ("atSign", "@")
  , ("openBracket", "[")
  , ("backslash", "/")
  , ("closeBracket", "]")
  , ("caret", "^")
  , ("underscore", "_")
  , ("backtick", "`")
  , ("openBrace", "{")
  , ("verticalBar", "|")
  , ("closeBrace", "}")
  , ("tilde", "~")
  ]

singleCharsWithUpperCase = map toTriple singleChars where
  toTriple (n, c) = (n, u, c) where
    u = let x:xs = n
        in C.toUpper x : xs

tokenList :: String
tokenList = concatMap toTok singleCharsWithUpperCase where
  toTok (n, u, _) = "    " ++ n ++ " { A." ++ u ++ " }\n"

main :: IO ()
main = putStr tokenList
