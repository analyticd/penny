module Main where

mkDigitsNonZero :: Int -> String
mkDigitsNonZero i = unlines [ dataLine, derivingLine, ""]
  where
    derivingLine = "  deriving (Eq, Ord, Show)"
    dataLine = "data D" ++ show i ++ " = "
      ++ intersperseBars (map mk [1..i])
      where
        mk d = "D" ++ show i ++ "'" ++ show d

mkDigits :: Bool -> Int -> String
mkDigits doZero i
  | not doZero && i == 0 = ""
  | otherwise = unlines $ [ dataLine, derivingLine, ""] ++ instanceLines
      ++ [""]
  where
    nm = "D" ++ show i ++ if doZero then "z" else ""
    ctor num = nm ++ "'" ++ show num
    derivingLine = "  deriving (Eq, Ord, Show)"
    list = if doZero then [0..i] else [1..i]
    dataLine = "data " ++ nm ++ " = "
      ++ intersperseBars (map ctor list)

    instanceLines =
      [ "instance Digit " ++ nm ++ " where"
      , "  digitToInt x = case x of"
      ] ++ digitToIntCases
      ++
      [ ""
      , "  intToDigit x = case x of"
      ] ++ intToDigitCases

    digitToIntCases = map mkCase list
      where
        mkCase num = "    " ++ ctor num
          ++ " -> " ++ show num

    intToDigitCases = map mkCase list
      ++ ["    _ -> Nothing"]
      where
        mkCase num = "    " ++ show num ++ " -> Just "
          ++ nm ++ "'" ++ show num

intersperseBars :: [String] -> String
intersperseBars xs = case xs of
  [] -> []
  x:[] -> x
  x:xs -> x ++ concatMap mk xs
    where
      mk i = (" | " ++ i)

mkBoth :: Int -> String
mkBoth i = mkDigits True i ++ mkDigits False i

main :: IO ()
main = do
  putStr . unlines $
    [ "module Penny.Lincoln.Rep.DigitsNew where"
    , ""
    , "class Digit a where"
    , "  digitToInt :: Integral b => a -> b"
    , "  intToDigit :: Integral b => b -> Maybe a"
    , ""
    ]
  mapM_ putStr . map mkBoth $ [0..9]
