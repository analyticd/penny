module Main where

import qualified Penny.Copper.Parsec as P
import qualified Data.Binary as B
import System.Environment (getArgs)

main :: IO ()
main = do
  (fn:_) <- getArgs
  (_, is) <- P.parse fn
  let fn' = fn ++ ".bin"
  B.encodeFile fn' is
