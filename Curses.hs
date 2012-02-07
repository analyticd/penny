module Main where

import System.Console.Terminfo
import Control.Monad

printBlue :: Capability TermOutput
printBlue = do
  f <- withForegroundColor `mplus` return (flip const)
  return (f (ColorNumber 202) (termText "Hello world!"))
    

main :: IO ()
main = do
  t <- setupTermFromEnv
  --t <- setupTerm "dumb"
  let c = getCapability t printBlue
  case c of
    Nothing -> putStrLn "Error!"
    Just c -> runTermOutput t c
