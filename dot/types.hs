module Main where

import qualified Types as T
import Text.Dot
import Prelude hiding
  ( maybe
  )

main :: IO ()
main = putStr . showDot $ do
  unit <- T.unit
  novem <- T.novem
  decem <- T.decem novem
  int <- T.int
  maybeInt <- T.maybe int
  orient <- T.orient
  return ()
