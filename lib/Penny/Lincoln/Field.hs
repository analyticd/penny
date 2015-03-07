{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Penny.Lincoln.Field where

import Data.Text (Text, pack)
import Penny.Lincoln.DateTime
import Penny.Lincoln.Display

data Tree = Tree Realm (Maybe Scalar) [Tree]
  deriving (Eq, Ord, Show)

scalarChild :: (Field a, Field b) => Realm -> a -> b -> Tree
scalarChild rlm s1 s2 = Tree rlm (Just $ toScalar s1)
  [Tree rlm (Just $ toScalar s2) []]


treeChildren :: Field a => Realm -> a -> [Tree] -> Tree
treeChildren rlm s1 = Tree rlm (Just $ toScalar s1)

data Realm = User | System
  deriving (Eq, Ord, Show)

data Scalar
  = Chars Text
  | SDate Date
  | STime Time
  | SZone Zone
  | SInt Integer
  deriving (Eq, Ord, Show)

scalarChars :: Scalar -> Maybe Text
scalarChars s = case s of { Chars x -> Just x; _ -> Nothing }

scalarDate :: Scalar -> Maybe Date
scalarDate s = case s of { SDate x -> Just x; _ -> Nothing }

scalarTime :: Scalar -> Maybe Time
scalarTime s = case s of {STime x -> Just x; _ -> Nothing }

scalarZone :: Scalar -> Maybe Zone
scalarZone s = case s of { SZone x -> Just x; _ -> Nothing }

scalarInteger :: Scalar -> Maybe Integer
scalarInteger s = case s of { SInt x -> Just x; _ -> Nothing }

displayScalar :: Scalar -> Text
displayScalar sc = case sc of
  Chars x -> x
  SDate d -> pack . display d $ ""
  STime ti -> pack . display ti $ ""
  SZone zn -> pack . display zn $ ""
  SInt i -> pack . display i $ ""

class Field a where
  toScalar :: a -> Scalar
  fromScalar :: Scalar -> Maybe a

instance Field Text where
  toScalar = Chars
  fromScalar s = case s of { Chars x -> Just x; _ -> Nothing }

instance Field Date where
  toScalar = SDate
  fromScalar s = case s of { SDate x -> Just x; _ -> Nothing }

instance Field Time where
  toScalar = STime
  fromScalar s = case s of {STime x -> Just x; _ -> Nothing }

instance Field Zone where
  toScalar = SZone
  fromScalar s = case s of { SZone x -> Just x; _ -> Nothing }

instance Field Integer where
  toScalar = SInt
  fromScalar s = case s of { SInt x -> Just x; _ -> Nothing }

memo :: Text
memo = "memo"

date :: Text
date = "date"

flag :: Text
flag = "flag"

number :: Text
number = "number"

payee :: Text
payee = "payee"

account :: Text
account = "account"

tags :: Text
tags = "tags"

fwdPstgGlobalSerial :: Text
fwdPstgGlobalSerial = "posting forward global serial"

revPstgGlobalSerial :: Text
revPstgGlobalSerial = "posting reverse global serial"

fwdPstgFileSerial :: Text
fwdPstgFileSerial = "posting forward file serial"

revPstgFileSerial :: Text
revPstgFileSerial = "posting reverse file serial"

fwdTxnGlobalSerial :: Text
fwdTxnGlobalSerial = "transaction forward global serial"

revTxnGlobalSerial :: Text
revTxnGlobalSerial = "transaction reverse global serial"

fwdTxnFileSerial :: Text
fwdTxnFileSerial = "transaction forward file serial"

revTxnFileSerial :: Text
revTxnFileSerial = "transaction reverse file serial"

fwdPstgIndexSerial :: Text
fwdPstgIndexSerial = "posting forward index serial"

revPstgIndexSerial :: Text
revPstgIndexSerial = "posting reverse index serial"

filename :: Text
filename = "filename"

lineNumber :: Text
lineNumber = "line number"
