{-# LANGUAGE OverloadedStrings #-}

module Penny.Semantic.Matcher where

import Control.Applicative
import Data.Monoid
import Penny.Matcher
import Penny.Display
import Penny.Semantic
import Data.String

equal
  :: (SemanticEq s, Display s, Monad m)
  => s
  -> Matcher s m s
equal tgt = do
  subj <- getSubject
  if semanticEq subj tgt
    then return subj
    else empty


greater
  :: (SemanticOrd s, Display s, Monad m)
  => s
  -> Matcher s m s
greater tgt = do
  subj <- getSubject
  if semanticOrd subj tgt == GT
    then return subj
    else empty

less
  :: (SemanticOrd s, Display s, Monad m)
  => s
  -> Matcher s m s
less tgt = do
  subj <- getSubject
  if semanticOrd subj tgt == LT
    then return subj
    else empty
