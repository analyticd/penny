{-# LANGUAGE TypeFamilies #-}
module Test where

-- Is this orthodox?  This associated type family is not indexed.

class C e where
  type Result
  get :: e Result

instance C Maybe where
  type Result = Int
  get = Just 3

-- How would you do it with a non-associated indexed type family?

type family ResultA a
type instance ResultA (Maybe a) = Int

class C' e where
  get' :: e Int

