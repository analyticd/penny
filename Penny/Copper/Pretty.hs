module Penny.Copper.Pretty where

import Penny.Lincoln.Pretty (Pretty(pretty))
import Text.PrettyPrint (text, (<+>))

import qualified Penny.Copper.Meta as M

instance Pretty M.PriceLine where
  pretty (M.PriceLine l) = text "Price line:" <+> pretty l 

instance Pretty M.PostingLine where
  pretty (M.PostingLine l) = text "Posting line:" <+> pretty l

instance Pretty M.TopMemoLine where
  pretty (M.TopMemoLine l) = text "Top memo line:" <+> pretty l

instance Pretty M.TopLineLine where
  pretty (M.TopLineLine l) = text "Top line line:" <+> pretty l

