module Day2 (partI, partII) where

import Control.Monad (guard)
import Data.List (minimumBy)
import Data.Maybe
import qualified Data.Map as Map

type ID = String

scan :: ID -> Map.Map Char Int
scan = foldr updateMap Map.empty
    where updateMap :: Char -> Map.Map Char Int -> Map.Map Char Int
          updateMap k m = Map.insert k (fromMaybe 0 (Map.lookup k m) + 1 ) m

check :: Map.Map Char Int -> (Int, Int)
check map = (checkNum map 2, checkNum map 3)
    where checkNum :: Map.Map Char Int -> Int -> Int
          checkNum m val = if foldr ( \ n accum -> (n == val) ||  accum ) False m then 1 else 0


checkSum :: [ID] -> Int
checkSum ids = uncurry (*) $ foldr (\ (a,b) (x,y) -> (a + x, b + y)) (0,0) $  map (check . scan) ids

readInput :: FilePath -> IO [ID]
readInput path = lines <$> readFile path

--fghij fguij -> (1, "fgij")
distance :: ID -> ID -> (Int, String)
distance a b = foldr (\ (x, y) (num, val) -> if x == y then (num, x : val) else (num + 1, val))  (0, "") $ zip a b

findAllAnwsers :: [ID] -> [(Int, String)]
findAllAnwsers ids = do
    x <- ids
    y <- ids
    guard (x /= y)
    return $ distance x y

findCorrectAnswer :: [(Int, String)] -> String
findCorrectAnswer = snd . minimumBy (\ (a, b) (c,d) -> compare a c)

partI :: IO ()
partI = do
    ids <- readInput "data/input2.txt"
    print $ checkSum ids

partII :: IO ()
partII = do
    ids <- readInput "data/input2.txt"
    print $ (findCorrectAnswer . findAllAnwsers) ids
