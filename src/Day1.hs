module Day1
    ( partI ,
    ) where

import Data.List

readInput :: FilePath -> IO [Int]
readInput path = ( map readNum . lines) <$> readFile path
        where readNum :: String -> Int
              readNum raw = if "-" `isPrefixOf` raw then read raw else read $ drop 1 raw

partI :: IO ()
partI = do
    nums <- readInput "data/input1.txt"
    let result = sum nums
    print result

compute :: Int -> [Int] -> Int
compute num nums = if length nums == 0 then num else num + (last nums)
