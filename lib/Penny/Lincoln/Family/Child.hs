{-# LANGUAGE DeriveGeneric #-}

module Penny.Lincoln.Family.Child where

import qualified Data.Binary as B
import GHC.Generics (Generic)

-- | A Child has at least one sibling and a parent.
data Child p c =
  Child { child :: c
        , sibling1 :: c
        , siblings :: [c]
        , parent :: p }
  deriving (Show, Generic)

instance (B.Binary p, B.Binary c) => B.Binary (Child p c)
