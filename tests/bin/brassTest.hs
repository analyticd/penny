module Main where

import Penny.Brass.Lexer (alexScanTokens)
import qualified Data.ByteString.Lazy as BS
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
  tokens <- fmap alexScanTokens BS.getContents
  putStr (ppShow tokens)
