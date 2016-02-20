{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Penny.Copper.Types where

import Penny.Copper.Grammar

import Pinchot (allRulesToTypes, makeOptics)

allRulesToTypes makeOptics ''Char [''Eq, ''Ord, ''Show] grammar
