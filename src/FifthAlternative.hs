module FifthAlternative (advent) where

import Data.Char

advent :: IO ()
advent = do
    input <- readFile "inputs/5.txt"
    print $ reactPolymer input

reactPolymer :: String -> Int
reactPolymer polymer =
    reactPolymer' [] polymer
    where
        reactPolymer' (x:xs) (y:ys) =
            if doReact x y
                then reactPolymer' xs ys
                else reactPolymer' (y:x:xs) ys
        reactPolymer' [] (y:ys) = reactPolymer' [y] ys
        reactPolymer' xs [] = length xs

doReact :: Char -> Char -> Bool
doReact x y = (toLower x == toLower y) && (isLower x /= isLower y)

