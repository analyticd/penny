{-# LANGUAGE OverloadedStrings #-}
module Penny.Field.Matcher where

import Control.Monad
import Penny.DateTime
import Penny.Matcher
import Penny.Field
import Data.Text (Text)

text :: MonadPlus m => Scalar -> m Text
text = (return . scalarChars) >=> just

date :: MonadPlus m => Scalar -> m Date
date = (return . scalarDate) >=> just

time :: MonadPlus m => Scalar -> m Time
time = (return . scalarTime) >=> just

zone :: MonadPlus m => Scalar -> m Zone
zone = (return . scalarZone) >=> just

integer :: MonadPlus m => Scalar -> m Integer
integer = (return . scalarInteger) >=> just

user :: MonadPlus m => Realm -> m ()
user = guard . (== User)

system :: MonadPlus m => Realm -> m ()
system = guard . (== System)

