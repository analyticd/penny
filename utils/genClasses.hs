module Main where

fileData :: String -> [String]
fileData = filter (not . bad) . lines
  where
    bad s = null s || head s == '#'

classesKind0 :: String -> [String]
classesKind0 s =
  [ "instance Parseable " ++ s ++ " where parser = p" ++ s
  , "instance Renderable " ++ s ++ " where render = r" ++ s
  , ""
  ]

classesGroupOnly :: String -> [String]
classesGroupOnly s =
  [ "instance ParseableG " ++ s ++ " where parserG = p" ++ s
  , "instance RenderableG " ++ s ++ " where renderG = r" ++ s
  , ""
  ]

classesRadixAndGrouper :: String -> [String]
classesRadixAndGrouper s =
  [ "instance ParseableRG " ++ s ++ " where parserRG = p" ++ s
  , "instance RenderableRG " ++ s ++ " where renderRG = r" ++ s
  , ""
  ]

classesRadixOnly :: String -> [String]
classesRadixOnly s =
  [ "instance ParseableR " ++ s ++ " where parserR = p" ++ s
  , "instance RenderableR " ++ s ++ " where renderR = r" ++ s
  , ""
  ]

genTHArbitrary :: String -> String
genTHArbitrary s = "$(derive makeArbitrary ''" ++ s ++ ")"

genProperty :: String -> [String]
genProperty s =
  [ "prop_" ++ s ++ " :: " ++ s ++ " -> Bool"
  , "prop_" ++ s ++ " = testR"
  , ""
  ]

output :: (String -> [String]) -> String -> IO ()
output printer fn = do
  txt <- fmap fileData . readFile $ fn
  mapM putStrLn . concat . map printer $ txt
  return ()

main :: IO ()
main = do
  txt <- fmap fileData . readFile $ "AstTypes"
  let r = concat $ fmap genProperty txt
  mapM putStrLn r
  return ()
