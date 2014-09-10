module Types where

import Text.Dot
import Prelude hiding
  ( maybe
  )

-- | Data constructor.
dacon
  :: String
  -- ^ Name for this data constructor.
  -> [Tycon]
  -- ^ Fields of the data constructor.
  -> Dot NodeId
dacon nm fs = do
  nid <- node [("label", nm), ("shape", "box")]
  let mkEdge dest = edge nid (tyId dest) []
  mapM_ mkEdge fs
  return nid

data Tycon = Tycon
  { tyName :: String
  , tyId :: NodeId
  } deriving Show

-- | Builds an entire type.
tycon
  :: String
  -- ^ Name for this type.
  -> [(String, [Tycon])]
  -- ^ Name for each data constructor, and its associated fields.
  -> Dot Tycon
tycon nm fs = do
  nid <- node [("label", nm), ("shape", "oval")]
  dacons <- mapM (uncurry dacon) fs
  let mkEdge dest = edge nid dest [("style", "dotted")]
  mapM_ mkEdge dacons
  return $ Tycon nm nid

-- | A type with no constructors.
opaque
  :: String
  -- ^ Name for this type
  -> Dot Tycon
opaque n = tycon n []

--
--
--

novem :: Dot Tycon
novem = tycon "Novem" . map f $ ([0..9] :: [Int])
  where
    f num = ('D' : show num, [])

decem
  :: Tycon
  -- ^ Novem
  -> Dot Tycon
decem tyc = tycon "Decem"
  [ ("D0", []), ("Nonem", [tyc]) ]

unit :: Dot Tycon
unit = tycon "()" [("()", [])]

maybe
  :: Tycon
  -- ^ Type constructor for the parameterized type
  -> Dot Tycon
maybe tyc = tycon lbl [("Nothing", []), ("Just", [tyc])]
  where
    lbl = "Maybe (" ++ tyName tyc ++ ")"

int :: Dot Tycon
int = opaque "Int"

string :: Dot Tycon
string = opaque "String"

-- | An empty data constructor.  The single argument is the name of
-- the constructor.
empty :: String -> Dot NodeId
empty s = dacon s []

orient :: Dot Tycon
orient
  = tycon "Orient"
  . map empty
  . map ("CommodityOn" ++)
  $ ["Left", "Right"]
