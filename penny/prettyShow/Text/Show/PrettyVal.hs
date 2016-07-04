{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
module Text.Show.PrettyVal ( PrettyVal(prettyVal) ) where

import Text.Show.Value

import Data.Ratio
import Data.Word
import Data.Int
import GHC.Generics

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified Data.Sequence as Seq

-- | A class for types that may be reified into a value.
-- Instances of this class may be derived automatically,
-- for datatypes that support `Generics`.
class PrettyVal a where
  prettyVal :: a -> Value
  listValue :: [a] -> Value

  default prettyVal :: (GDump (Rep a), Generic a) => a -> Value
  prettyVal = oneVal . gdump . from

  default listValue :: [a] -> Value
  listValue = List . map prettyVal


class GDump f where
  gdump :: f a -> [(Name,Value)]

instance GDump U1 where
  gdump U1 = []

instance (GDump f, GDump g) => GDump (f :*: g) where
  gdump (xs :*: ys) = gdump xs ++ gdump ys

instance (GDump f, GDump g) => GDump (f :+: g) where
  gdump (L1 x) = gdump x
  gdump (R1 x) = gdump x

instance PrettyVal a => GDump (K1 t a) where
  gdump (K1 x) = [ ("", prettyVal x) ]

instance (GDump f, Datatype d) => GDump (M1 D d f) where
  gdump (M1 x) = gdump x

instance (GDump f, Constructor c) => GDump (M1 C c f) where
  gdump c@(M1 x)
    | conIsRecord c = [ ("", Rec   name (gdump x)) ]
    | isTuple name  = [ ("", Tuple (map snd (gdump x))) ]
    | otherwise     = [ ("", Con   name (map snd (gdump x))) ]

    where
    name = conName c

    isTuple ('(' : cs) = case span (== ',') cs of
                           (_,")") -> True
                           _ -> False
    isTuple _          = False

instance (GDump f, Selector s) => GDump (M1 S s f) where
  gdump it@(M1 x) = repeat (selName it) `zip` map snd (gdump x)

oneVal :: [(Name,Value)] -> Value
oneVal x =
  case x of
    [ ("",v) ]               -> v
    fs | all (null . fst) fs -> Con "?" (map snd fs)
       | otherwise           -> Rec "?" fs


mkNum :: (Ord a, Num a, Show a) => (String -> Value) -> a -> Value
mkNum c x
  | x >= 0    = c (show x)
  | otherwise = Neg (c (show (negate x)))

instance PrettyVal Int     where prettyVal   = mkNum Integer
instance PrettyVal Integer where prettyVal   = mkNum Integer
instance PrettyVal Float   where prettyVal x = Float (show x)
instance PrettyVal Double  where prettyVal x = Float (show x)

instance PrettyVal Word8   where prettyVal x = Integer (show x)
instance PrettyVal Word16  where prettyVal x = Integer (show x)
instance PrettyVal Word32  where prettyVal x = Integer (show x)
instance PrettyVal Word64  where prettyVal x = Integer (show x)

instance PrettyVal Int8    where prettyVal   = mkNum Integer
instance PrettyVal Int16   where prettyVal   = mkNum Integer
instance PrettyVal Int32   where prettyVal   = mkNum Integer
instance PrettyVal Int64   where prettyVal   = mkNum Integer

instance PrettyVal Char    where
  prettyVal x    = Char (show x)
  listValue xs = String xs

instance PrettyVal a => PrettyVal [a] where
  prettyVal xs   = listValue xs

instance (PrettyVal a, Integral a) => PrettyVal (Ratio a) where
  prettyVal r = Ratio (prettyVal (numerator r)) (prettyVal (denominator r))

instance (PrettyVal a1, PrettyVal a2) => PrettyVal (a1,a2)
instance (PrettyVal a1, PrettyVal a2, PrettyVal a3) => PrettyVal (a1,a2,a3)
instance (PrettyVal a1, PrettyVal a2, PrettyVal a3,
          PrettyVal a4) => PrettyVal (a1,a2,a3,a4)

instance (PrettyVal a1, PrettyVal a2, PrettyVal a3,
          PrettyVal a4, PrettyVal a5) => PrettyVal (a1,a2,a3,a4,a5)

instance (PrettyVal a1, PrettyVal a2, PrettyVal a3,
          PrettyVal a4, PrettyVal a5, PrettyVal a6) => PrettyVal (a1,a2,a3,a4,a5,a6)

instance (PrettyVal a1, PrettyVal a2, PrettyVal a3,
          PrettyVal a4, PrettyVal a5, PrettyVal a6, PrettyVal a7)
  => PrettyVal (a1,a2,a3,a4,a5,a6,a7)

instance PrettyVal Bool
instance PrettyVal Ordering
instance PrettyVal a => PrettyVal (Maybe a)
instance (PrettyVal a, PrettyVal b) => PrettyVal (Either a b)

instance PrettyVal Text where prettyVal = String . X.unpack

instance PrettyVal () where prettyVal _ = Con "()" []

-- # Data.Time
instance PrettyVal Time.Day where
  prettyVal d = Con "Data.Time.Day" [String . show $ d]
instance PrettyVal Time.TimeOfDay where
  prettyVal d = Con "Data.Time.TimeOfDay" [String . show $ d]
instance PrettyVal Time.ZonedTime where
  prettyVal d = Con "Data.Time.ZonedTime" [String . show $ d]

-- # Seq

instance PrettyVal a => PrettyVal (Seq.Seq a) where
  prettyVal d = Con "Data.Sequence.Seq"
    [List . toList . fmap prettyVal $ d]

-- # Map

instance (Ord k, PrettyVal a, PrettyVal k) => PrettyVal (Map k a) where
  prettyVal m = Con "Data.Map.Map"
    [List . fmap prettyVal . Map.assocs $ m]
