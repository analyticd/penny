{-# LANGUAGE OverloadedStrings #-}
module Penny.Field.Matcher where

import Control.Monad
import Penny.DateTime
import Penny.Matcher
import Penny.Field
import Data.Text (Text)

text :: MonadPlus m => Scalar -> m Text
text = (pure . scalarChars) >=> just

date :: MonadPlus m => Scalar -> m Date
date = (pure . scalarDate) >=> just

time :: MonadPlus m => Scalar -> m Time
time = (pure . scalarTime) >=> just

zone :: MonadPlus m => Scalar -> m Zone
zone = (pure . scalarZone) >=> just

integer :: MonadPlus m => Scalar -> m Integer
integer = (pure . scalarInteger) >=> just

user :: MonadPlus m => Realm -> m ()
user = guard . (== User)

system :: MonadPlus m => Realm -> m ()
system = guard . (== System)

