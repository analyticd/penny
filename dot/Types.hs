module Types where

import Dot
import Data.List (intersperse)
import Prelude hiding
  ( maybe
  , product
  , either
  , seq
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

-- | A type with one empty constructor.
nullary :: String -> Dot Tycon
nullary s = tycon s [(s, [])]

-- | an entire type with just one constructor; it will have the same
-- name as the type.  A newtype.
oneConst
  :: String
  -- ^ Name for this type
  -> Tycon
  -- ^ The contained type.
  -> Dot Tycon
oneConst n ty = tycon n [(n, [ty])]

-- | Product types.  The constructor will have the same name as the type.
product
  :: String
  -- ^ Name for this type
  -> [Tycon]
  -- ^ Name for each field
  -> Dot Tycon
product n fs = tycon n [(n, fs)]

-- | A type with no constructors.
opaque
  :: String
  -- ^ Name for this type
  -> Dot Tycon
opaque n = tycon n []

-- | An empty data constructor.  The single argument is the name of
-- the constructor.
empty :: String -> (String, [a])
empty s = (s, [])

-- | Labels for parameterized types.
paralabel
  :: String
  -- ^ Type name
  -> [String]
  -- ^ Label for each type
  -> String
paralabel n = concat . intersperse " " . (n:) . map f
  where
    f s = "(" ++ s ++ ")"

-- | Labels for tuples.
tuplabel
  :: [String]
  -- ^ Label for each type
  -> String
tuplabel ls = "(" ++ (concat . intersperse ", " $ ls) ++ ")"

tuple2
  :: Tycon
  -> Tycon
  -> Dot Tycon
tuple2 x y = product (tuplabel [tyName x, tyName y]) [x, y]

--
-- Primitives and non-Penny types
--

bool :: Dot Tycon
bool = tycon "Bool" [empty "True", empty "False"]

either
  :: Tycon
  -- ^ Left
  -> Tycon
  -- ^ Right
  -> Dot Tycon
either l r = tycon lbl [("Left", [l]), ("Right", [r])]
  where
    lbl = paralabel "Either" [tyName l, tyName r]

int :: Dot Tycon
int = opaque "Int"

maybe
  :: Tycon
  -- ^ Type constructor for the parameterized type
  -> Dot Tycon
maybe tyc = tycon lbl [("Nothing", []), ("Just", [tyc])]
  where
    lbl = paralabel "Maybe" [tyName tyc]

seq
  :: String
  -- ^ Name of parameterized type
  -> Dot Tycon
seq = opaque . paralabel "Seq" . (:[])

string :: Dot Tycon
string = opaque "String"

unit :: Dot Tycon
unit = tycon "()" [("()", [])]

--
-- Penny types
--

arithmeticError
  :: Tycon
  -- ^ String
  -> Dot Tycon
arithmeticError = oneConst "ArithmeticError.T"

arrangement
  :: Tycon
  -- ^ Orient
  -> Tycon
  -- ^ SpaceBetween
  -> Dot Tycon
arrangement o s = product "Arrangement.T" [o, s]

decem
  :: Tycon
  -- ^ Novem
  -> Dot Tycon
decem tyc = tycon "Decem"
  [ ("D0", []), ("Nonem", [tyc]) ]

decems :: Dot Tycon
decems = seq "Decem"

grouper
  :: Tycon
  -- ^ Unique
  -> Dot Tycon
grouper un = tycon (paralabel "Grouper.T" [tyName un])
  [ empty "Space"
  , empty "Thin"
  , empty "Under"
  , ("Unique", [un])
  ]

ng1
  :: Tycon
  -- ^ Radix.T
  -> Tycon
  -- ^ Zeroes.T
  -> Tycon
  -- ^ Grouper
  -> Tycon
  -- ^ Seq (ZGroup.T r)
  -> Dot Tycon
ng1 r z g s = product (paralabel "NG1.T" [tyName g])
  [r,z,g,z,s]

nilUngrouped
  :: String
  -- ^ Type name of parameterized type
  -> Tycon
  -- ^ Znu1
  -> Tycon
  -- ^ RadZ
  -> Dot Tycon
nilUngrouped ty z r = tycon (paralabel "NilUngrouped.T" [ty])
  [ ("NilUngrouped.LeadingZero", [z])
  , ("NilUngrouped.NoLeadingZero", [r])]

nodecs3
  :: Tycon
  -- ^ NovDecs
  -> Tycon
  -- ^ SeqDecs
  -> Dot Tycon
nodecs3 n s = product "Nodecs3.T" [n, s]

nonZero :: Dot Tycon
nonZero = opaque "NonZero.T"

novDecs
  :: Tycon
  -- ^ Novem
  -> Tycon
  -- ^ Decems
  -> Dot Tycon
novDecs n d = product "NovDecs.T" [n, d]

novem :: Dot Tycon
novem = tycon "Novem" . map f $ ([1..9] :: [Int])
  where
    f num = ('D' : show num, [])

orient :: Dot Tycon
orient
  = tycon "Orient.T"
  . map empty
  . map ("Orient.CommodityOn" ++)
  $ ["Left", "Right"]

radix
  :: String
  -- ^ Name of parameterized type
  -> Dot Tycon
radix s = tycon lbl [("Radix.T", [])]
  where
    lbl = "Radix.T " ++ s

radun
  :: String
  -- ^ Parameterized type
  -> Tycon
  -- ^ Radix
  -> Tycon
  -- ^ Maybe Zeroes
  -> Dot Tycon
radun ty r z = product (paralabel "Radun.T" [ty]) [r, z]

radZ
  :: String
  -- ^ Parameterized type
  -> Tycon
  -- ^ Radix
  -> Tycon
  -- ^ Zeroes
  -> Dot Tycon
radZ ty r z = product (paralabel "RadZ.T" [ty]) [r, z]

seqDecs :: Dot Tycon
seqDecs = seq "DecsGroup.T"

spaceBetween
  :: Tycon
  -- ^ Boolean
  -> Dot Tycon
spaceBetween = oneConst "SpaceBetween.T"

zeroes
  :: Tycon
  -- ^ NonZero
  -> Dot Tycon
zeroes = oneConst "Zeroes.T"

zero :: Dot Tycon
zero = nullary "Zero.T"

znu1
  :: String
  -- ^ Name of parameterized type
  -> Tycon
  -- ^ Zero
  -> Tycon
  -- ^ Maybe Radun.T
  -> Dot Tycon
znu1 ty z r = product (paralabel "Znu1.T" [ty]) [z, r]
