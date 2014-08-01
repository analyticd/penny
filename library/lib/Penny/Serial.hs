{-# LANGUAGE DeriveGeneric #-}

module Penny.Serial
  ( Serial
  , forward
  , backward
  , serialItems
  , serialSomeItems
  , serialNestedItems
  ) where

import Control.Applicative (Applicative, (<*>), pure, (*>))
import Control.Monad (ap, liftM)
import Data.Traversable (Traversable)
import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fdbl

data SerialSt = SerialSt
  { nextFwd :: Int
  , nextBack :: Int
  } deriving Show


data Serial = Serial
  { forward :: Int
  , backward :: Int
  } deriving (Eq, Show, Ord)

newtype GenSerial a = GenSerial (SerialSt -> (a, SerialSt))

instance Functor GenSerial where
  fmap = liftM

instance Applicative GenSerial where
  pure = return
  (<*>) = ap

instance Monad GenSerial where
  return a = GenSerial $ \s -> (a, s)
  (GenSerial k) >>= f = GenSerial $ \s ->
    let (a, s') = k s
        GenSerial g = f a
    in g s'

incrementBack :: GenSerial ()
incrementBack = GenSerial $ \s ->
  let s' = SerialSt (nextFwd s) (nextBack s + 1)
  in ((), s')

getSerial :: GenSerial Serial
getSerial = GenSerial $ \s ->
  let s' = SerialSt (nextFwd s + 1) (nextBack s - 1)
  in (Serial (nextFwd s) (nextBack s), s')

makeSerials :: GenSerial a -> a
makeSerials (GenSerial k) =
  let (r, _) = k (SerialSt 0 0) in r

serialItems :: Traversable t => (Serial -> a -> b) -> t a -> t b
serialItems f c = makeSerials $ do
  _ <- Tr.traverse (const incrementBack) c
  let k a = do
        s <- getSerial
        return $ f s a
  Tr.traverse k c

serialSomeItems
  :: Traversable t
  => (a -> Either b (Serial -> b))
  -> t a
  -> t b
serialSomeItems f as = makeSerials k
  where
    k = do
      let doIncr i = case f i of
            Left _ -> return ()
            Right _ -> incrementBack
      _ <- Tr.mapM doIncr as
      let addSer i = case f i of
            Left b -> return b
            Right add -> getSerial >>= return . add
      Tr.mapM addSer as

-- | Adds serials to items that are nested within other items.
serialNestedItems
  :: (Traversable f, Traversable t)
  => (a -> Either b ((f c), (Serial -> c -> d), (f d -> b)))
  -- ^ When applied to each item, this function returns Left if the
  -- item does not need a serial, or Right if it has items that need
  -- serials. In the Right is the container with items that need
  -- serials, the function that applies serials to each item, and a
  -- function to re-wrap the container with the serialed items.

  -> t a
  -> t b
serialNestedItems getEi as = makeSerials k
  where
    k = do
      serialNestedIncrBack getEi as
      Tr.mapM (serialNestedAddSerials getEi) as

-- | Increments the back serial by the needed number of items.
serialNestedIncrBack
  :: (Fdbl.Foldable f, Tr.Traversable t)
  => (a -> Either b (f c, x, y))
  -> t a
  -> GenSerial ()
serialNestedIncrBack f t = Tr.mapM doIncr t *> pure () where
  doIncr i = case f i of
    Left _ -> return ()
    Right (ctnr, _, _) ->
      let len = length . Fdbl.toList $ ctnr
      in (Tr.sequenceA . replicate len $ incrementBack)
         *> pure ()

-- | Assigns serials to nested items.
serialNestedAddSerials
  :: Tr.Traversable f
  => (a -> Either b (f c, (Serial -> c -> d), f d -> b))
  -> a
  -> GenSerial b
serialNestedAddSerials f a = case f a of
  Left b -> return b
  Right (ctnr, addSer, rewrap) -> do
    let adder i = do
          s <- getSerial
          return $ addSer s i
    fmap rewrap $ Tr.mapM adder ctnr
