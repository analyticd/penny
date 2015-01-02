{-# LANGUAGE OverloadedStrings #-}
module Penny.Lincoln.Field where

import Data.Text
import Data.Sequence (Seq)
import Penny.Lincoln.Decimal
import Penny.Lincoln.DateTime

newtype Label = Label Text
  deriving (Eq, Ord, Show)

data Payload
  = PayDate DateTime
  | PayText Text
  | PayTexts (Seq Text)
  | PayDecimal Decimal
  deriving (Eq, Ord, Show)

data Field
  = Field Label Payload
  -- ^ @Field a b@, where
  --
  -- @a@ is the /label/ for the field, and
  --
  -- @b@ is the 'Payload', the data the field contains.
  deriving (Eq, Ord, Show)

memo :: Label
memo = Label "memo"

date :: Label
date = Label "date"

flag :: Label
flag = Label "flag"

number :: Label
number = Label "number"

payee :: Label
payee = Label "payee"

account :: Label
account = Label "account"

tags :: Label
tags = Label "tags"

fwdPstgGlobalSerial :: Label
fwdPstgGlobalSerial = Label "posting forward global serial"

revPstgGlobalSerial :: Label
revPstgGlobalSerial = Label "posting reverse global serial"

fwdPstgFileSerial :: Label
fwdPstgFileSerial = Label "posting forward file serial"

revPstgFileSerial :: Label
revPstgFileSerial = Label "posting reverse file serial"

fwdTxnGlobalSerial :: Label
fwdTxnGlobalSerial = Label "transaction forward global serial"

revTxnGlobalSerial :: Label
revTxnGlobalSerial = Label "transaction reverse global serial"

fwdTxnFileSerial :: Label
fwdTxnFileSerial = Label "transaction forward file serial"

revTxnFileSerial :: Label
revTxnFileSerial = Label "transaction reverse file serial"

fwdPstgIndexSerial :: Label
fwdPstgIndexSerial = Label "posting forward index serial"

revPstgIndexSerial :: Label
revPstgIndexSerial = Label "posting reverse index serial"

filename :: Label
filename = Label "filename"

lineNumber :: Label
lineNumber = Label "line number"
