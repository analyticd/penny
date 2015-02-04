module Main where

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
mkBoth i
  = mkDigits True i
  ++ mkToChar True i
  ++ mkDigits False i
  ++ (if i > 0 then mkToChar False i else "")
  ++ mkConv i
  ++ mkRevConv i

mkConv :: Int -> String
mkConv i
  | i == 0 = ""
  | otherwise = unlines lns
  where
    sig = nm ++ " :: " ++ from ++ " -> " ++ to
    from = "D" ++ show i
    to = from ++ "z"
    nm = "c'" ++ to ++ "'" ++ from
    firstLine = nm ++ " x = case x of"
    restLines = map ("  " ++) . map maker $ [1..i]
    maker num = from ++ "'" ++ show num ++ " -> " ++
      to ++ "'" ++ show num
    lns = [sig, firstLine] ++ restLines ++ [""]

mkRevConv :: Int -> String
mkRevConv i
  | i == 0 = ""
  | otherwise = unlines lns
  where
    sig = nm ++ " :: " ++ from ++ " -> " ++ "Maybe " ++ to
    from = "D" ++ show i ++ "z"
    to = "D" ++ show i
    nm = "c'" ++ to ++ "'" ++ from
    firstLine = nm ++ " x = case x of"
    restLines = map ("  " ++) . map maker $ [0..i]
    maker num = from ++ "'" ++ show num ++ " -> " ++ mayCtor num
      where
        mayCtor num'
          | num' == 0 = "Nothing"
          | otherwise = "Just " ++ to ++ "'" ++ show num
    lns = [sig, firstLine] ++ restLines ++ [""]

mkToChar :: Bool -> Int -> String
mkToChar doZero i = unlines $ sig : firstLine : rest ++ [""]
  where
    nm = "D" ++ show i ++ if doZero then "z" else ""
    ctor n = nm ++ "'" ++ show n
    caseLine n = "  " ++ ctor n ++ " -> '" ++ show n ++ "'"
    fName = "c'Char'" ++ nm
    sig = fName ++ " :: " ++ nm ++ " -> Char"
    firstLine = fName ++ " x = case x of"
    list = if doZero then [0..i] else [1..i]
    rest = map caseLine list

main :: IO ()
main = do
  putStr . unlines $
    [ "module Penny.Lincoln.Rep.Digits where"
    , ""
    , "class Digit a where"
    , "  digitToInt :: Integral b => a -> b"
    , "  intToDigit :: Integral b => b -> Maybe a"
    , ""
    ]
  mapM_ putStr . map mkBoth $ [0..9]
