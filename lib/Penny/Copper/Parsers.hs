{-# LANGUAGE TemplateHaskell #-}
module Penny.Copper.Parsers where

import qualified Pinchot
import qualified Penny.Copper.Types as Ty
import qualified Penny.Copper.Grammar as G

Pinchot.allEarleyGrammars "Ty" G.grammar
