module Penny.Brass.Scanner where

import qualified Data.ByteString as BS
import qualified Penny.Brass.AlexInput as A
import qualified Penny.Brass.Lexer as L

data Result a =
  Ok a
  | Failed String

type Line = Int
type Col = Int

data State = State {
  remaining :: !BS.ByteString
  , lineNum :: Line
  , colNum :: Col
  }

data Location = Location !Line !Col

newtype StateM a =
  StateM { runStateM :: State -> (Result a, State) }

location :: StateM Location
location = StateM $ \s ->
  (Ok (Location (lineNum s) (colNum s)), s)

inject :: a -> StateM a
inject a = StateM $ \s -> (Ok a, s)

displayLocation :: Location -> String
displayLocation (Location lin col) =
  "line: " ++ show lin ++ " column: " ++ show col

displayToken :: A.Token -> String
displayToken = show

parseError :: A.Token -> StateM a
parseError tok = do
  loc <- location
  let s = "parse error at " ++ displayLocation loc
          ++ " bad token: " ++ displayToken tok
          ++ "\n"
  fail s


bind :: StateM a -> (a -> StateM b) -> StateM b
bind (StateM fa) f = StateM $ \s ->
  case fa s of
    (Failed bad, s') -> (Failed bad, s')
    (Ok r, s') -> let (StateM fb) = f r
               in fb s'

instance Monad StateM where
  return = inject
  (>>=) = bind
  fail s = StateM $ \st -> (Failed s, st)

data InnerResult a =
  Skipped !BS.ByteString !Col
  | InnerEOF (StateM a)
  | InnerError
  | FoundToken !BS.ByteString (StateM a) !Line !Col

lexerInner ::
  (A.Token -> StateM a)
  -> BS.ByteString
  -> Line
  -> Col
  -> InnerResult a
lexerInner cont bs lin col = case L.alexScan bs 0 of
  L.AlexToken rest len act -> let
    str = BS.take len bs
    tok = act str
    (lin', col') = case tok of
      A.Newline -> (lin + 1, col)
      _ -> (lin, col + len)
    in FoundToken rest (cont tok) lin' col'
  L.AlexSkip bs' len -> Skipped bs' (col + len)
  L.AlexEOF ->
    InnerEOF (cont A.EOF)
  L.AlexError _ ->
    InnerError


lexer :: (A.Token -> StateM a) -> StateM a
lexer cont = StateM $ \s -> f s where
  f st@(State bs li co) = case lexerInner cont bs li co of
    FoundToken bs' k lin col ->
      let newSt = st { remaining = bs'
                     , lineNum = lin
                     , colNum = col }
      in runStateM k newSt
    Skipped bs' col ->
      let newSt = st { remaining = bs'
                     , colNum = col }
      in f newSt
    InnerEOF st' ->
      runStateM st' (st { remaining = BS.empty })
    InnerError -> (Failed "parse error", st)
