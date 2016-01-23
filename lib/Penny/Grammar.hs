{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Penny.Grammar where

import Penny.Pinchot

import Pinchot (allRulesToTypes, makeOptics)

allRulesToTypes makeOptics ''Char [''Eq, ''Ord, ''Show] grammar
