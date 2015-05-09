{-# LANGUAGE OverloadedStrings #-}
module Penny.Field.Matcher where

import Penny.DateTime
import Penny.Matcher
import Penny.Field
import Data.Text (Text)

text
  :: Monad m
  => Matcher Text m a
  -> Matcher Scalar m a
text = labelNestMaybe "text" (fmap return scalarChars)

date
  :: Monad m
  => Matcher Date m a
  -> Matcher Scalar m a
date = labelNestMaybe "date" (fmap return scalarDate)

time
  :: Monad m
  => Matcher Time m a
  -> Matcher Scalar m a
time = labelNestMaybe "time" (fmap return scalarTime)

zone
  :: Monad m
  => Matcher Zone m a
  -> Matcher Scalar m a
zone = labelNestMaybe "zone" (fmap return scalarZone)

integer
  :: Monad m
  => Matcher Integer m a
  -> Matcher Scalar m a
integer = labelNestMaybe "integer" (fmap return scalarInteger)

