module Penny.Zinc.Parser.Filter (
  parseFilter
  , NeedsHelp(NeedsHelp)
  , Result(Result, resultFactory, resultSensitive, sorterFilterer)
  ) where

import Control.Applicative ((<$>), many)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Monoid (mempty, mappend)
import Data.Text (Text)
import qualified Text.Matchers.Text as M
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as X

import qualified Penny.Zinc.Parser.Defaults as D
import qualified Penny.Zinc.Parser.Defaults as Defaults

-- | Parses all filtering options. If the parse contains any errors, abort.
parseFilter :: Defaults.T -> Parser (Either NeedsHelp Result)
parseFilter d = fmap f (many parser) where
  f ls =
    let k = foldl (flip (.)) id ls
        s = k (newState d)
    in if help s
       then Left NeedsHelp
       else case Ly.parsePredicate . tokens $ s of
         Nothing -> Ly.abort "could not parse filter expression"
         Just pdct ->
           let fn = Ly.xactionsToFiltered pdct
                    (postFilter s) (orderer s)
               r = Result { resultFactory = factory s
                          , resultSensitive = sensitive s
                          , sorterFilterer = fn }
           in Right $ r


-- | Returned if the user requested help.
data NeedsHelp = NeedsHelp
                 deriving Show

-- | Indicates the result of a successful parse of filtering options.
data Result =
  Result { resultFactory :: M.CaseSensitive
                            -> Text -> Ex.Exceptional Text (Text -> Bool)
           -- ^ The factory indicated, so that it can be used in
           -- subsequent parses of the same command line.

         , resultSensitive :: M.CaseSensitive
           -- ^ Indicated case sensitivity, so that it can be used in
           -- subsequent parses of the command line.
           
         , sorterFilterer :: [L.Transaction] -> [L.Box Ly.LibertyMeta]
           -- ^ Applied to a list of Transaction, will sort and filter
           -- the transactions and assign them LibertyMeta.
         }


data State =
  State { sensitive :: M.CaseSensitive
        , factory :: M.CaseSensitive
                     -> Text -> Ex.Exceptional Text (Text -> Bool)
        , tokens :: [X.Token (L.PostFam -> Bool)]
        , postFilter :: [Ly.PostFilterFn]
        , orderer :: Ly.Orderer
        , help :: Bool
        , currentTime :: L.DateTime }

newState ::
  Defaults.T
  -> State
newState d =
  State { sensitive = D.sensitive d
        , factory = D.factory d
        , tokens = []
        , postFilter = []
        , orderer = mempty
        , help = False
        , currentTime = D.currentTime d }

parser :: Parser (State -> State)
parser = C.parseOption $
         operand
         ++ parsePostFilter
         ++ parseMatcherSelect
         ++ parseCaseSelect
         ++ parseOperator
         ++ [parseSort, parseHelp]


operand :: [C.OptSpec (State -> State)]
operand = map (fmap f) Ly.operandSpecs
  where
    f lyFn st =
      let (X.Operand op) =
            lyFn (currentTime st) (sensitive st) (factory st)
      in st { tokens = tokens st ++ [X.TokOperand op] }
                   
parsePostFilter :: [C.OptSpec (State -> State)]
parsePostFilter = map (fmap f) [s1, s2]
  where
    (s1, s2) = Ly.postFilterSpecs
    f g st = st { postFilter = postFilter st ++ [g] }

parseMatcherSelect :: [C.OptSpec (State -> State)]
parseMatcherSelect = map (fmap f) Ly.matcherSelectSpecs
  where
    f fty st = st { factory = fty }

parseCaseSelect :: [C.OptSpec (State -> State)]
parseCaseSelect = map (fmap f) Ly.caseSelectSpecs
  where
    f sel st = st { sensitive = sel }

parseOperator :: [C.OptSpec (State -> State)]
parseOperator = map (fmap f) Ly.operatorSpecs
  where
    f tok st = st { tokens = tokens st ++ [tok] }

parseSort :: C.OptSpec (State -> State)
parseSort = f <$> Ly.sortSpecs
  where
    f ord st = st { orderer = mappend ord (orderer st) }


parseHelp :: C.OptSpec (State -> State)
parseHelp = C.OptSpec ["help"] ['h'] (C.NoArg f)
  where
    f st = st { help = True }
