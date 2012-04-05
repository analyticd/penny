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

lexer :: (A.Token -> StateM a) -> StateM a
lexer cont = StateM $ \s -> f s where
  f st = let
    res = runStateM (lexerInner cont) st
    in case res of
      Failed str -> Failed str
      Ok bs' maySt -> case maySt of
        Nothing -> f bs'
        (Just k') -> runStateM k' bs'

lexerInner ::
  (A.Token -> StateM a)
  -> StateM (Maybe (StateM a))
lexerInner cont = StateM $ \bs ->
  case L.alexScan bs 0 of
    L.AlexSkip bs' _ -> Ok bs' Nothing
    L.AlexEOF ->
      Ok BS.empty (Just $ cont A.EOF)
    L.AlexError _ ->
      Ok BS.empty (Just (fail "lexer error"))
    L.AlexToken rest len act -> let
      str = BS.take len bs
      tok = act str
      in Ok rest (Just $ cont tok)
