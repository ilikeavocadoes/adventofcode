module Second (advent) where

import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Map (fromListWith, toList)
import Data.Maybe (fromJust)

advent :: IO ()
advent = do
    input <- TIO.readFile "inputs/3.txt"
    part1 input
    part2 input

part1 :: T.Text -> IO ()
part1 input = do
    let (twices, thrices) = foldl accumulate23s (0,0) $ T.lines input
    print $ twices * thrices

part2 :: T.Text -> IO ()
part2 input = do
    let allPairs = [ (x, y) | x <- T.lines input, y <- T.lines input ]
        differingByPair = map (\(x, y) -> differsBy1 x y) allPairs
        fabricBoxes = fromJust $ lookup True $ zip differingByPair allPairs
        (sameletters, _) = unzip $ filter (\(x, y) -> x == y) $ T.zip (fst fabricBoxes) (snd fabricBoxes)
    print sameletters
    
differsBy1 :: T.Text -> T.Text -> Bool
differsBy1 t1 t2 =
    let pairs = T.zip t1 t2
        sameLetters = map (\(x, y) -> x == y) pairs
    in 1 == length (filter not sameLetters) 

accumulate23s :: (Integer, Integer) -> T.Text -> (Integer, Integer)
accumulate23s (twices, thrices) id =
    ( if hasNsame 2 id
          then twices + 1
          else twices
    , if hasNsame 3 id
          then thrices + 1
          else thrices
    )

hasNsame :: Integer -> T.Text -> Bool
hasNsame n id =
    let (letters, occurrences) = unzip $ counts id
    in n `elem` occurrences

counts :: T.Text -> [(Char, Integer)]
counts id = toList $ fromListWith (+) [(x, 1) | x <- T.unpack id]
