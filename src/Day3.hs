module Day3 where

import Text.ParserCombinators.Parsec
import Data.Either (isRight)
data Claim = Claim { no :: Int
                   , left :: Int
                   , top :: Int
                   , width :: Int
                   , height :: Int
                   } deriving (Show)

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
    return Claim {no=(read claimNo), left=(read leftNum), top=(read topNum), width=(read widthNum ), height = (read heightNum)}

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseInput :: String -> Either ParseError [Claim]
parseInput input = parse claimFile "(unknown)" input

parseInputIO :: FilePath -> IO (Either ParseError [Claim])
parseInputIO path = do
    content <-  readFile path
    return $ parseInput content
