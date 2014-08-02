module Penny.TopLine.Shrinkers where

import Penny.TopLine
import Penny.Common.Shrinkers
import Prelude.Shrinkers
import Penny.DateTime.Shrinkers
import Penny.Serial.Shrinkers
import Prelude hiding (maybe)

topLineData :: TopLineData -> [TopLineData]
topLineData (TopLineData d m n f p) =
  [ TopLineData d' m' n' f' p' | (d', m', n', f', p') <-
    tuple5 dateTime memo number flag payee (d, m, n, f, p) ]

topLineMeta :: TopLineMeta -> [TopLineMeta]
topLineMeta (TopLineMeta m l g f n) =
  [ TopLineMeta m' l' g' f' n' | (m', l', g', f', n') <-
    tuple5 line line serial serial filename (m, l, g, f, n) ]

topLine :: TopLine -> [TopLine]
topLine (TopLine t m) =
  [ TopLine t' m' | (t', m') <-
    tuple2 topLineData (maybe topLineMeta) (t, m) ]
