module Main where

import Description
import Distribution.PackageDescription.PrettyPrint

main :: IO ()
main = putStrLn . showGenericPackageDescription $ genericDescription
