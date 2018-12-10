{-# LANGUAGE RecordWildCards #-}
module Day3
    ( partI ,
      partII
    ) where

import           Control.Monad                 (join)
import           Data.Either                   (isRight)
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Text.ParserCombinators.Parsec

data Claim = Claim { no :: Int
                   , left :: Int
                   , top :: Int
                   , width :: Int
                   , height :: Int
                   } deriving (Show)

type Square = (Int, Int)

claimFile :: GenParser Char st [Claim]
claimFile =
    do result <- many line
       eof
       return  result

line :: GenParser Char st Claim
line = do
    char '#'
    claimNo <- many digit
    spaces
    char '@'
    spaces
    leftNum <- many digit
    char ','
    topNum <- many digit
    char ':'
    spaces
    widthNum <- many digit
    char 'x'
    heightNum <- many digit
    eol
    return Claim {no=(read claimNo), left=(read leftNum), top=(read topNum), width=(read widthNum ), height = read heightNum}

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseInput :: String -> Either ParseError [Claim]
parseInput = parse claimFile "(unknown)"

parseInputIO :: FilePath -> IO (Either ParseError [Claim])
parseInputIO path = do
    content <-  readFile path
    return $ parseInput content

getClaimArea :: Claim -> [Square]
getClaimArea claim = [(l + i, t + j)| i <- [0 .. (w - 1)],
                                      j <- [0 .. (h - 1)]]
    where t = top    claim
          l = left   claim
          w = width  claim
          h = height claim

-- RecordWildCards version
coords :: Claim -> [Square]
coords Claim {..} = do
    x <- [left .. left + width - 1]
    y <- [top .. top + height - 1]
    pure (x, y)

countAreaAsMap :: [Square] -> Map.Map Square Int
countAreaAsMap = Map.fromListWith (+) . map (\ x -> (x, 1))

countArea :: [Square] -> (Set.Set Square, Set.Set Square)
countArea = foldr f (Set.empty, Set.empty)
    where  f :: Square -> (Set.Set Square, Set.Set Square) -> (Set.Set Square, Set.Set Square)
           f square (seen, overlap) = if Set.member square seen then (seen, Set.insert square overlap) else (Set.insert square seen, overlap)

getOverlapped :: [Claim] -> Int
getOverlapped = Set.size . snd . countArea . join . map getClaimArea

getOverlapped' :: [Claim] -> Int
getOverlapped' = Map.foldr (\ a b -> if a >= 2 then b + 1 else b) 0 . countAreaAsMap . join . map getClaimArea

partI :: IO ()
partI = do
    parseResult <- parseInputIO "data/testInput3.txt"
    case parseResult of
        Left e -> print e >> fail "parser error"
        Right claims -> print $ getOverlapped claims


getClaimAreaTupe :: Claim -> [(Square , Int)]
getClaimAreaTupe claim = zip (getClaimArea claim) (repeat $ no claim)

countAreaAsMapSet :: [Claim] -> Map.Map Square (Set.Set Int)
countAreaAsMapSet = Map.fromListWith Set.union . map (\ (a, b) -> (a, Set.singleton b) ) . join .map getClaimAreaTupe

getOverlapClaim :: [Claim] -> Set.Set Int
getOverlapClaim = Set.fromList . join . map Set.toList . filter ((> 1) . Set.size) . Map.elems .  countAreaAsMapSet

getAllClaimNo :: [Claim] -> Set.Set Int
getAllClaimNo = Set.fromList . map no

partII :: IO ()
partII = do
    parseResult <- parseInputIO "data/input3.txt"
    case parseResult of
        Left e -> print e >> fail "parser error"
        Right claims -> print $ Set.difference (getAllClaimNo claims)  (getOverlapClaim claims)
