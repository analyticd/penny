module Penny.Zinc.Expressions.Queues (
  Back,
  emptyBack,
  enqueue,
  Front,
  front,
  emptyFront,
  View(Empty, (:<)),
  view,
  push) where

data Back t = Back [t]
            deriving Show

emptyBack :: Back t
emptyBack = Back []

enqueue :: t -> Back t -> Back t
enqueue t (Back ts) = Back (t:ts)

data Front t = Front [t]
             deriving Show

front :: Back t -> Front t
front (Back ts) = Front (reverse ts)

emptyFront :: Front t
emptyFront = Front []

data View t =
  Empty
  | t :< Front t
  deriving Show

view :: Front t -> View t
view (Front ts) = case ts of
  [] -> Empty
  t:ts -> t :< (Front ts)

push :: t -> Front t -> Front t
push t (Front ts) = Front (t:ts)
