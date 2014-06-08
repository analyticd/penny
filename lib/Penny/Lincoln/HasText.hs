{-# LANGUAGE FlexibleInstances #-}
module Penny.Lincoln.HasText where

import Data.Text (Text)

class HasText a where
  text :: a -> Text

instance HasText Text where
  text = id

class HasTextList a where
  textList :: a -> [Text]

instance HasTextList [Text] where
  textList = id
