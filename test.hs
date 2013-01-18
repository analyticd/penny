module Main where

import Control.Exception (evaluate)

main = main3

data OList a = Nil | Cons a (OList a)
  deriving Show

seqList :: [a] -> ()
seqList [] = ()
seqList (x:xs) = x `seq` seqList xs

bigList :: [(OList Int, OList Int)]
bigList =
  iterate (\(x, y) -> (Cons undefined x, Cons undefined y))
  (Nil, Nil)

main1 :: IO ()
main1 =  evaluate . seqList . take 10000000 $ bigList

-- | This heap profile is a big list of OList, too
bigList2 :: [(OList Int, OList Int)]
bigList2 = iterate f (Nil, Nil)
  where f (x, y) = (add1 x, add1 y)
        add1 Nil = Cons 0 Nil
        add1 l@(Cons e _) = let n = e + 1 in n `seq` Cons n l

-- | this heap profile generates a huge pile of OList
main2 :: IO ()
main2 = evaluate . seqList . take 10000000 $ bigList2

--
-- The code below generates a pile of Int and a bigger pile of SList
--

data SList a = SNil | SCons !a !(SList a)
  deriving Show

data SPair a b = SPair !a !b
  deriving Show

bigList3 :: [SPair (SList Int) (SList Int)]
bigList3 = iterate f (SPair SNil SNil)
  where f (SPair x y) = SPair (add1 x) (add1 y)
        add1 SNil = SCons 0 SNil
        add1 l@(SCons e _) = SCons (e + 1) l

main3 :: IO ()
main3 = evaluate . seqList . take 10000000 $ bigList3

