{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}
module Test where

type family R (a :: * -> *) :: *
type instance R Maybe = Int

class C' a where
  -- type family R a
  getInt' :: a Int
  getBool' :: R a -> a Bool

instance C' Maybe where
  -- type R Maybe = Int
  getInt' = Just 3
  getBool' i = Just $ i < 10

printer :: IO ()
printer = print $ (getBool' 5 :: Maybe Bool)

class C a b | a -> b where
  getInt :: a Int
  getBool :: b -> a Bool

instance C Maybe Int where
  getInt = Just 3
  getBool i = Just $ i < 10

