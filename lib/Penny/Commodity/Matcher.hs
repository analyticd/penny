{-# LANGUAGE OverloadedStrings #-}
module Penny.Commodity.Matcher where

import Penny.Matcher
import Data.Text (Text)
import Penny.Commodity

commodityName
  :: Monad m
  => Matcher Text m a
  -> Matcher Commodity m a
commodityName mtcr = do
  inform "running Text matcher on commodity name"
  Commodity txt <- getSubject
  indent $ study mtcr txt

