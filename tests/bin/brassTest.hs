module Main where

import Data.List (foldl')
import Penny.Brass.Lexer (alexScanTokens)
import qualified Data.ByteString.Lazy as BS
--import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
  tokens <- fmap alexScanTokens BS.getContents
  let folder acc e = e `seq` succ acc
      folded = foldl' folder (0 :: Int) tokens
  putStrLn (show folded)
