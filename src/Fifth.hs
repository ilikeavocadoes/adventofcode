module Fifth (advent) where

import Data.Char

advent :: IO ()
advent = do
    input <- readFile "inputs/5.txt"
    print $ iterativelyReact input
    print $ findShortestAlternative input

findShortestAlternative polymer =
    minimum $ map (iterativelyReact . (removeLetters polymer)) ['a','b'..'z']

removeLetters polymer letter = filter ((/= letter) . toLower) polymer

iterativelyReact :: String -> Int
iterativelyReact polymer =
    let reactedLengths = map length $ iterate react polymer
    in findSameValues reactedLengths

findSameValues :: [Int] -> Int
findSameValues [x] = x
findSameValues (x:y:xs) = if x == y then x else findSameValues (y:xs)

react :: String -> String
react [] = []
react (x:[]) = x:[]
react (x:y:xs) = if sameLetter x y && differentCase x y
                   then react xs
                   else x : react (y:xs)

sameLetter :: Char -> Char -> Bool
sameLetter x y = (toLower x) == (toLower y)

differentCase :: Char -> Char -> Bool
differentCase x y = isLower x /= isLower y
