{-# LANGUAGE OverloadedStrings #-}

module Sixth (advent) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative
import qualified Data.Attoparsec.Text as P
import Data.List
import Data.Ord (comparing)

advent :: IO ()
advent = do
    input <- TIO.readFile "inputs/6.txt"
    let coords = parseCoordinates input
        namedCoords = zip [1..] coords
        xmin = minimum $ map fst coords
        ymin = minimum $ map snd coords
        xmax = maximum $ map fst coords
        ymax = maximum $ map snd coords
        coordMap = [[closestCoord namedCoords (x, y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
        names = fst $ unzip namedCoords
        areas = map (count (concat coordMap)) names
        areasWithoutInfinite = filter (notOnEdge coordMap . fst) $ zip names areas
    print $ maximumBy (comparing snd) areasWithoutInfinite
    let sumDistanceMap = [[distanceToAll coords (x, y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
    print $ length $ filter (< 10000) $ concat sumDistanceMap

distanceToAll :: [(Integer, Integer)] -> (Integer, Integer) -> Integer
distanceToAll coords point = sum $ map (manhattan point) coords

count xs x = length . filter (x==) $ xs

notOnEdge :: [[Integer]] -> Integer -> Bool
notOnEdge coordMap name = not $ name `elem` edges
    where edges = concat
            [ head coordMap
            , last coordMap
            , map head coordMap
            , map last coordMap
            ]

closestCoord :: [(Integer, (Integer, Integer))] -> (Integer, Integer) -> Integer
closestCoord coords point =
    let distances = map ((manhattan point) . snd) coords 
        names = fst $ unzip coords
        (x:y:_) = sortBy (comparing snd) $ zip names distances
    in if snd x == snd y then 0 else fst x

manhattan :: (Integer, Integer) -> (Integer, Integer) -> Integer
manhattan (x, y) (v, w) = abs (x - v) + abs (y - w)

parseCoordinates :: T.Text -> [(Integer, Integer)]
parseCoordinates input =
    let result = P.parseOnly (many coordParser) input
    in case result of
        Left _ -> []
        Right coords -> coords

coordParser :: P.Parser (Integer, Integer)
coordParser = do
    x <- P.decimal
    P.string ", "
    y <- P.decimal
    P.endOfLine
    return (x, y)
