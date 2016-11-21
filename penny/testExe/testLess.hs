{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
-- | Tests the @toLess@ function in "Penny.Stream".
--
-- Expected output:
--
-- @less@ appears with numbers from 0 to 1000 appearing, one number on
-- each line.

module Main where

import Penny.Stream (toLess, streamToStdin)

import Data.Sequence (fromList)
import qualified Data.Text as X
import Rainbow (chunk, byteStringMakerFromEnvironment)

main :: IO ()
main = do
  maker <- byteStringMakerFromEnvironment
  let chunks = fmap (chunk . flip X.snoc '\n' . X.pack . show)
        . fromList $ [0..1000]
      str = streamToStdin toLess
  str maker chunks
