module Day4 where

import           Data.List (sort)
import           Data.Time
import           Text.ParserCombinators.Parsec

data Entry = MKEntry { timestamp :: LocalTime
                     , action :: Action
                     } deriving (Show)

data Action = WakesUp | FallsAsleep | BeginShift Int deriving (Show, Eq)

instance  Eq Entry  where
    (MKEntry a b) == (MKEntry c d) = (a == c) && (b == d)

instance Ord Entry where
    compare p@(MKEntry a b) q@(MKEntry c d)
            | p == q = EQ
            | otherwise = compare a c

--https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) 0
              }

shiftParser :: Parser Action
shiftParser = do
    string "Guard"
    spaces
    char '#'
    id <- many digit
    spaces
    string "begins shift"
    return $ BeginShift (read id)

actionParser :: Parser Action
actionParser =
      (string "falls asleep" >> return FallsAsleep)
  <|> (string "wakes up" >> return WakesUp)
  <|> shiftParser


entryParser :: Parser Entry
entryParser = do
    char '['
    timestamp <- timeParser
    char ']'
    spaces
    action <- actionParser
    char '\n'
    return $ MKEntry timestamp action

entryFile :: Parser [Entry]
entryFile =
    do result <- many entryParser
       eof
       return result

parseInput :: String -> Either ParseError [Entry]
parseInput = parse entryFile "(unknown)"

parseInputIO :: FilePath -> IO (Either ParseError [Entry])
parseInputIO path = do
    content <-  readFile path
    return $ parseInput content

partI :: IO ()
partI = do
    parseResult <- parseInputIO "data/input4.txt"
    case parseResult of
        Left e -> print e >> fail "parser error"
        Right entries -> mapM_ (putStrLn . show ) $ sort entries
