{-# LANGUAGE OverloadedStrings #-}
module Penny.Field.Matcher where

import Penny.DateTime
import Penny.Matcher
import Penny.Field
import Data.Text (Text)

text :: Monad m => Matcher Scalar m Text
text = labelNest "text" (fmap return scalarChars) just

date :: Monad m => Matcher Scalar m Date
date = labelNest "date" (fmap return scalarDate) just

time :: Monad m => Matcher Scalar m Time
time = labelNest "time" (fmap return scalarTime) just

zone :: Monad m => Matcher Scalar m Zone
zone = labelNest "zone" (fmap return scalarZone) just

integer :: Monad m => Matcher Scalar m Integer
integer = labelNest "integer" (fmap return scalarInteger) just

