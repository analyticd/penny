{-# LANGUAGE OverloadedStrings #-}
module Penny.Commodity.Matcher where

import Penny.Matcher
import Data.Text (Text)
import Penny.Commodity

commodity
  :: Monad m
  => Matcher Text m a
  -> Matcher Commodity m a
commodity mtcr = do
  inform "running Text matcher on commodity name"
  Commodity txt <- getSubject
  indent $ study mtcr txt

