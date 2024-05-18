module Utils where

prettyPrint :: (Show a) => [Char] -> [([Char], a)] -> [Char]
prettyPrint title keyValues = result
  where
    str = title:" ":[a ++ ": " ++ (show b) ++ ", " | (a, b) <- keyValues]
    result = init $ init $ concat str
