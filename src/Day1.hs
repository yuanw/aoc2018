module Day1
    ( partI  ,
      partII ,
    ) where

import Control.Monad.Trans.State.Lazy
import Data.List

readInput :: FilePath -> IO [Int]
readInput  = (map readNum . lines <$>) . readFile
        where readNum :: String -> Int
              readNum raw = if "-" `isPrefixOf` raw then read raw else read $ drop 1 raw

partI :: IO ()
partI = do
    nums <- readInput "data/input1.txt"
    let result = sum nums
    print result

-- TODO :: maybe try State --
partII :: IO ()
partII = do
    nums <- readInput "data/input1.txt"
    let seq = cycle nums
    let result = findRepeat seq [0]
    print result


compute :: Int -> [Int] -> Int
compute num nums = if null nums then num else num + head nums

update :: Int -> [Int] -> (Bool, [Int])
update num nums = (newNum `elem` nums, newNum : nums)
    where newNum = compute num nums

findRepeat :: [Int] -> [Int] -> Int
findRepeat seq nums = if exist then head newNums else findRepeat (tail seq) newNums
    where num = head seq
          (exist, newNums) = update num nums
