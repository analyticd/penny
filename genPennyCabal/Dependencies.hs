module Dependencies where

import Cartel

base :: Package
base = closedOpen "base" [4,9,0,0] [5]

anonymousSums :: Package
anonymousSums = atLeast "anonymous-sums" [0,4,0,0]

bytestring :: Package
bytestring = atLeast "bytestring" [0,10,4]

text :: Package
text = atLeast "text" [1,2]

containers :: Package
containers = atLeast "containers" [0,5]

time :: Package
time = atLeast "time" [1,4]

transformers :: Package
transformers = atLeast "transformers" [0,3]

quickcheck :: Package
quickcheck = atLeast "QuickCheck" [2,7,6]

tasty :: Package
tasty = atLeast "tasty" [0,11]

tastyHunit :: Package
tastyHunit = atLeast "tasty-hunit" [0,9,2]

tastyQuickcheck :: Package
tastyQuickcheck = atLeast "tasty-quickcheck" [0,8,3]

tastyTh :: Package
tastyTh = atLeast "tasty-th" [0,1,3]

bifunctors :: Package
bifunctors = atLeast "bifunctors" [4,2]

rainbox :: Package
rainbox = atLeast "rainbox" [0,18]

rainbow :: Package
rainbow = atLeast "rainbow" [0,26]

semigroups :: Package
semigroups = atLeast "semigroups" [0,16,1]

contravariant :: Package
contravariant = atLeast "contravariant" [1,2]

pipes :: Package
pipes = atLeast "pipes" [4,1,4]

pipesSafe :: Package
pipesSafe = atLeast "pipes-safe" [2,2]

-- | Currently unused
process :: Package
process = atLeast "process" [1,2,0,0]

async :: Package
async = atLeast "async" [2,0]

turtle :: Package
turtle = atLeast "turtle" [1,0,2]

mtl :: Package
mtl = atLeast "mtl" [2,2,1]

logict :: Package
logict = atLeast "logict" [0,6]

void :: Package
void = atLeast "void" [0,7]

lens :: Package
lens = atLeast "lens" [4,9]

operational :: Package
operational = atLeast "operational" [0,2,3]

pretty :: Package
pretty = atLeast "pretty" [1,1,2]

derive :: Package
derive = atLeast "derive" [2,5,22]

managed :: Package
managed = atLeast "managed" [1,0]

hspec :: Package
hspec = atLeast "hspec" [2,2]

monoidSubclasses :: Package
monoidSubclasses = atLeast "monoid-subclasses" [0,4,1]

earley :: Package
earley = atLeast "Earley" [0,10,1,0]

pinchot :: Package
pinchot = atLeast "pinchot" [0,22,0,0]

accuerr :: Package
accuerr = atLeast "accuerr" [0,2,0,0]

ofx :: Package
ofx = atLeast "ofx" [0,4,2,0]

parsec :: Package
parsec = atLeast "parsec" [3,1,9]

timelens :: Package
timelens = atLeast "timelens" [0,2]

prettyShow :: Package
prettyShow = atLeast "pretty-show" [1,6]

templateHaskell :: Package
templateHaskell = atLeast "template-haskell" [2,10]

nonEmptySequence :: Package
nonEmptySequence = atLeast "non-empty-sequence" [0,2]

optparseApplicative :: Package
optparseApplicative = atLeast "optparse-applicative" [0,12]
