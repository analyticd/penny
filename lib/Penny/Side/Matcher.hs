{-# LANGUAGE OverloadedStrings #-}
module Penny.Side.Matcher where

import Control.Applicative
import Penny.Side
import Penny.Matcher

debit :: Monad m => Matcher Side m ()
debit = getSubject >>= f
  where
    f s | s == Debit = return ()
        | otherwise = empty

credit :: Monad m => Matcher Side m ()
credit = getSubject >>= f
  where
    f s | s == Credit = return ()
        | otherwise = empty
