{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Penny.Clatcher where

import Control.Exception
import Penny.Clatch
import Penny.Price
import Penny.Serial
import Penny.Tree
import Data.Typeable
import Data.Sequence (Seq)

-- | Describes any errors that may arise in the clatcher.
data PennyError
  = ParseError String
  -- ^ A file failed to parse.
  deriving (Show, Typeable)

instance Exception PennyError

-- | Apply the first function to the global transaction serial to get
-- a 'Serpack'.  Apply the second list of functions to the respective
-- global posting serials to get a 'Posting'.
type Pretrans = (Serset -> Serpack, (Seq Tree, Seq (Serset -> Posting)))

class Loader a where
  loadTransactions :: a -> IO (Seq Price, Seq Transaction)

data LoadScroll
  = Preloaded (Seq Price) (Seq Pretrans)
  | OpenFile String

instance Loader (Seq LoadScroll) where
  loadTransactions = undefined
