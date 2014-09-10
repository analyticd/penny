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
  let mkEdge dest = edge nid (tyNodeId dest) []
  mapM_ mkEdge fs
  return nid

data Typename = Typename
  { tyId :: String
  , tyParam :: [Typename]
  } deriving (Eq, Ord, Show)

-- | Type with no parameterized types.
kind1 :: String -> Typename
kind1 s = Typename s []

typenameToString :: Typename -> String
typenameToString (Typename s ts)
  = concat
  . intersperse " "
  . (s:)
  . map (\n -> "(" ++ n ++ ")")
  . map typenameToString
  $ ts

data Tycon = Tycon
  { tyName :: Typename
  , tyNodeId :: NodeId
  } deriving Show


-- | Builds an entire type.
tycon
  :: Typename
  -> [(String, [Tycon])]
  -- ^ Name for each data constructor, and its associated fields.
  -> Dot Tycon
tycon nm fs = do
  nid <- node [("label", typenameToString nm), ("shape", "oval")]
  dacons <- mapM (uncurry dacon) fs
  let mkEdge dest = edge nid dest [("style", "dotted")]
  mapM_ mkEdge dacons
  return $ Tycon nm nid

-- | A type with one empty constructor.
nullary
  :: Typename
  -> Dot Tycon
nullary s = tycon s [(tyId s, [])]

-- | an entire type with just one constructor; it will have the same
-- name as the type.  A newtype.
oneConst
  :: Typename
  -> Tycon
  -- ^ The contained type.
  -> Dot Tycon
oneConst n ty = tycon n [(tyId n, [ty])]

-- | Product types.  The constructor will have the same name as the type.
product
  :: Typename
  -> [Tycon]
  -- ^ Name for each field
  -> Dot Tycon
product n fs = tycon n [(tyId n, fs)]

-- | A type with no constructors.
opaque
  :: Typename
  -> Dot Tycon
opaque n = tycon n []

-- | An empty data constructor.  The single argument is the name of
-- the constructor.
empty :: String -> (String, [a])
empty s = (s, [])

-- | Labels for tuples.
tuplabel
  :: [Typename]
  -> Typename
tuplabel ls =
  Typename ( "(" ++ (concat . intersperse ", " . map typenameToString $ ls)
             ++ ")") []

tuple2
  :: Tycon
  -> Tycon
  -> Dot Tycon
tuple2 x y = product (tuplabel . map tyName $ [x, y]) [x, y]

-- | Builds a Typename that is parameterized on the same type as a
-- child Tycon.
copyUp
  :: String
  -- ^ Name of this type
  -> Tycon
  -- ^ Child type
  -> Typename
copyUp n = Typename n . tyParam . tyName

--
-- Primitives and non-Penny types
--

bool :: Dot Tycon
bool = tycon (kind1 "Bool") [empty "True", empty "False"]

either
  :: Tycon
  -- ^ Left
  -> Tycon
  -- ^ Right
  -> Dot Tycon
either l r = tycon (Typename "Either" (map tyName [l, r]))
  [("Left", [l]), ("Right", [r])]

int :: Dot Tycon
int = opaque (kind1 "Int")

maybe
  :: Tycon
  -- ^ Type constructor for the parameterized type
  -> Dot Tycon
maybe tyc = tycon (Typename "Maybe" [tyName tyc])
  [("Nothing", []), ("Just", [tyc])]

seq
  :: Typename
  -- ^ Name of parameterized type
  -> Dot Tycon
seq = opaque . Typename "Seq" . (:[])

string :: Dot Tycon
string = opaque (kind1 "String")

unit :: Dot Tycon
unit = nullary (kind1 "()")

--
-- Penny types
--

arithmeticError
  :: Tycon
  -- ^ String
  -> Dot Tycon
arithmeticError = oneConst (kind1 "ArithmeticError.T")

arrangement
  :: Tycon
  -- ^ Orient
  -> Tycon
  -- ^ SpaceBetween
  -> Dot Tycon
arrangement o s = product (kind1 "Arrangement.T") [o, s]

decem
  :: Tycon
  -- ^ Novem
  -> Dot Tycon
decem tyc = tycon (kind1 "Decem")
  [ empty "D0", ("Nonem", [tyc]) ]

decems :: Dot Tycon
decems = seq (kind1 "Decem")

grouper
  :: Tycon
  -- ^ Unique
  -> Dot Tycon
grouper un = tycon (Typename "Grouper.T" [tyName un])
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
ng1 r z g s = product (copyUp "NG1.T" r) [r,z,g,z,s]


nilUngrouped
  :: Tycon
  -- ^ Znu1
  -> Tycon
  -- ^ RadZ
  -> Dot Tycon
nilUngrouped z r = tycon (copyUp "NilUngrouped.T" r)
  [ ("NilUngrouped.LeadingZero", [z])
  , ("NilUngrouped.NoLeadingZero", [r])]

nodecs3
  :: Tycon
  -- ^ NovDecs
  -> Tycon
  -- ^ SeqDecs
  -> Dot Tycon
nodecs3 n s = product (kind1 "Nodecs3.T") [n, s]

nonZero :: Dot Tycon
nonZero = opaque (kind1 "NonZero.T")

novDecs
  :: Tycon
  -- ^ Novem
  -> Tycon
  -- ^ Decems
  -> Dot Tycon
novDecs n d = product (kind1 "NovDecs.T") [n, d]

novem :: Dot Tycon
novem = tycon (kind1 "Novem") . map f $ ([1..9] :: [Int])
  where
    f num = ('D' : show num, [])

orient :: Dot Tycon
orient
  = tycon (kind1 "Orient.T")
  . map empty
  . map ("Orient.CommodityOn" ++)
  $ ["Left", "Right"]

radix
  :: Typename
  -- ^ Name of parameterized type
  -> Dot Tycon
radix s = nullary (Typename "Radix.T" [s])


radun
  :: Tycon
  -- ^ Radix
  -> Tycon
  -- ^ Maybe Zeroes
  -> Dot Tycon
radun r z = product (copyUp "Radun.T" r) [r, z]

radZ
  :: Tycon
  -- ^ Radix
  -> Tycon
  -- ^ Zeroes
  -> Dot Tycon
radZ r z = product (copyUp "Radun.T" r) [r, z]

seqDecs :: Dot Tycon
seqDecs = seq (kind1 "DecsGroup.T")

spaceBetween
  :: Tycon
  -- ^ Boolean
  -> Dot Tycon
spaceBetween = oneConst (kind1 "SpaceBetween.T")

zeroes
  :: Tycon
  -- ^ NonZero
  -> Dot Tycon
zeroes = oneConst (kind1 "Zeroes.T")

zero :: Dot Tycon
zero = nullary (kind1 "Zero.T")

znu1
  :: Tycon
  -- ^ Zero
  -> Tycon
  -- ^ Maybe Radun.T
  -> Dot Tycon
znu1 z r = product (copyUp "Znu1.T" r) [z, r]
