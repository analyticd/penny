{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Penny.Lincoln.Field where

import Data.Text
import Penny.Lincoln.DateTime

data Tree = Tree Scalar [Tree]
  deriving (Eq, Ord, Show)

scalarChild :: (Field a, Field b) => a -> b -> Tree
scalarChild s1 s2 = Tree (scalar s1) [Tree (scalar s2) []]


treeChildren :: Field a => a -> [Tree] -> Tree
treeChildren s1 = Tree (scalar s1)

data Scalar
  = Chars Text
  | SDate Date
  | STime Time
  | SZone Zone
  | SInt Integer
  | Binary Bool
  deriving (Eq, Ord, Show)

class Field a where
  scalar :: a -> Scalar

instance Field Scalar where
  scalar = id

instance Field String where
  scalar = Chars . pack

instance Field Text where
  scalar = Chars

instance Field Date where
  scalar = SDate

instance Field Time where
  scalar = STime

instance Field Zone where
  scalar = SZone

instance Field Integer where
  scalar = SInt

instance Field Bool where
  scalar = Binary

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
