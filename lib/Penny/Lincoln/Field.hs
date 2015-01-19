{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Penny.Lincoln.Field where

import Data.Text
import Penny.Lincoln.Decimal
import Penny.Lincoln.DateTime
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Tree = Tree Scalar Forest
  deriving (Eq, Ord, Show)

newtype Forest = Forest (Seq Tree)
  deriving (Eq, Ord, Show)

instance Monoid Forest where
  mempty = Forest Seq.empty
  mappend (Forest x) (Forest y) = Forest (x <> y)

scalarChild :: (Field a, Field b) => a -> b -> Tree
scalarChild s1 s2 = Tree (scalar s1)
  (Forest (Seq.singleton (Tree (scalar s2) (Forest Seq.empty))))

treeChildren :: Field a => a -> Seq Tree -> Tree
treeChildren s1 = Tree (scalar s1) . Forest

data Scalar
  = Chars Text
  | Date DateTime
  | Number Decimal
  | Binary Bool
  | Null
  deriving (Eq, Ord, Show)

class Field a where
  scalar :: a -> Scalar

instance Field Scalar where
  scalar = id

instance Field String where
  scalar = Chars . pack

instance Field Text where
  scalar = Chars

instance Field DateTime where
  scalar = Date

instance Field Decimal where
  scalar = Number

instance Field Integer where
  scalar = Number . fromInteger

instance Field Bool where
  scalar = Binary

instance Field () where
  scalar () = Null

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
