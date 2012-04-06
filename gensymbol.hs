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
  , ("equals", "=")
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

drCr = ["dr", "debit", "cr", "credit"]

singleCharsWithUpperCase = map toTriple singleChars where
  toTriple (n, c) = (n, u, c) where
    u = let x:xs = n
        in C.toUpper x : xs

tokenList :: String
tokenList = concatMap toTok singleCharsWithUpperCase where
  toTok (n, u, _) = "    " ++ n ++ " { A." ++ u ++ " }\n"

toText :: String
toText = concatMap toLine singleCharsWithUpperCase where
  toLine (n, u, c) =
    n ++ " :: X.Text\n"
    ++ n ++ " = X.singleton '" ++ c ++ "'\n\n"

commentContent :: String
commentContent = concatMap toLine singleCharsWithUpperCase where
  toLine (n, _, _) =
    "  | " ++ n ++ " { T.CommentText T." ++ n ++ " }\n"

singlesWithDrCr :: [(String, String)]

main :: IO ()
main = putStr commentContent
