{-# LANGUAGE OverloadedStrings #-}
module Penny.Side.Matcher where

import Penny.Side
import Penny.Matcher

debit :: Monad m => Matcher Side m ()
debit = getSubject >>= f
  where
    f s | s == Debit = proclaim "is Debit" >> accept ()
        | otherwise = proclaim "is Credit" >> reject

credit :: Monad m => Matcher Side m ()
credit = getSubject >>= f
  where
    f s | s == Debit = proclaim "is Debit" >> reject
        | otherwise = proclaim "is Credit" >> accept ()
