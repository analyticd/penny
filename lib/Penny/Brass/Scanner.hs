module Penny.Brass.Scanner where

import qualified Data.ByteString as BS
import qualified Penny.Brass.AlexInput as A
import qualified Penny.Brass.Lexer as L

data StateRes a =
  Ok !BS.ByteString !a
  | Failed String

newtype StateM a =
  StateM { runStateM :: BS.ByteString -> StateRes a }

inject :: a -> StateM a
inject a = StateM $ \bs -> Ok bs a

parseError :: A.Token -> StateM a
parseError _ = StateM $ \_ -> Failed "parse error"

bind :: StateM a -> (a -> StateM b) -> StateM b
bind (StateM fa) f = StateM $ \s ->
  case fa s of
    Failed bad -> Failed bad
    Ok s' a -> let (StateM fb) = f a
               in fb s'

instance Monad StateM where
  return = inject
  (>>=) = bind
  fail s = StateM $ \_ -> Failed s

data InnerResult a =
  Skipped !BS.ByteString
  | InnerEOF (StateM a)
  | InnerError
  | FoundToken !BS.ByteString (StateM a)


lexerInner ::
  (A.Token -> StateM a)
  -> BS.ByteString
  -> InnerResult a
lexerInner cont bs = case L.alexScan bs 0 of
  L.AlexToken rest len act -> let
    str = BS.take len bs
    tok = act str
    in FoundToken rest (cont tok)
  L.AlexSkip bs' _ -> Skipped bs'
  L.AlexEOF ->
    InnerEOF (cont A.EOF)
  L.AlexError _ ->
    InnerError


lexer :: (A.Token -> StateM a) -> StateM a
lexer cont = StateM $ \s -> f s where
  f bs = case lexerInner cont bs of
    FoundToken bs' st' -> runStateM st' bs'
    Skipped bs' -> f bs'
    InnerEOF st' -> runStateM st' BS.empty
    InnerError -> Failed "parse error"
