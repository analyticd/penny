{-# LANGUAGE OverloadedStrings #-}
module Penny.Field.Matcher where

import Penny.DateTime
import Penny.Matcher
import Penny.Field
import Data.Text (Text)

text :: Monad m => Matcher Scalar m Text
text = tunnel (return . scalarChars) `feed` just

date :: Monad m => Matcher Scalar m Date
date = tunnel (return . scalarDate) `feed` just

time :: Monad m => Matcher Scalar m Time
time = tunnel (return . scalarTime) `feed` just

zone :: Monad m => Matcher Scalar m Zone
zone = tunnel (return . scalarZone) `feed` just

integer :: Monad m => Matcher Scalar m Integer
integer = tunnel (return . scalarInteger) `feed` just

user :: Monad m => Matcher Realm m ()
user = tunnel (return . (== User)) `feed` true

system :: Monad m => Matcher Realm m ()
system = tunnel (return . (== System)) `feed` true
