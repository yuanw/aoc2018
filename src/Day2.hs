module Day2 (partI) where

import Data.Maybe
import qualified Data.Map as Map

type ID = String

scan :: ID -> Map.Map Char Int
scan id = foldr updateMap Map.empty id
    where updateMap :: Char -> Map.Map Char Int -> Map.Map Char Int
          updateMap k m = Map.insert k (fromMaybe 0 (Map.lookup k m) + 1 ) m

check :: Map.Map Char Int -> (Int, Int)
check map = (checkNum map 2, checkNum map 3)
    where checkNum :: Map.Map Char Int -> Int -> Int
          checkNum m val = if foldr ( \ n accum -> (n == val) ||  accum ) False m then 1 else 0


checkSum :: [ID] -> Int
checkSum ids = (\ (a, b) -> a * b) $ foldr (\ (a,b) (x,y) -> (a + x, b + y)) (0,0) $  map (check . scan) ids

readInput :: FilePath -> IO [ID]
readInput path = lines <$> readFile path

partI :: IO ()
partI = do
    ids <- readInput "data/input2.txt"
    print $ checkSum ids
