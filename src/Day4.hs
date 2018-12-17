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

splitByGuard :: [Entry] -> [[Entry]]
splitByGuard entires = foldl f [] entires
    where f :: [[Entry]] -> Entry -> [[Entry]]
          f ls e = case e of s@(MKEntry a (BeginShift x)) -> ls ++ [[e]]
                             _ -> (if (length ls < 2) then [] else (init ls)) ++ [((last ls) ++ [e])]

diffMin :: LocalTime -> LocalTime -> Integer
diffMin a b = ((* 1440) $ diffDays (localDay a) (localDay b)) + (toInteger $ (* 60) $ (todHour $ localTimeOfDay a) - (todHour $ localTimeOfDay b)) + (toInteger $ ((todMin $ localTimeOfDay a) - (todMin $ localTimeOfDay b)))

countSleep :: [Entry] -> Integer
countSleep [] = 0
countSleep (MKEntry a FallsAsleep : MKEntry b WakesUp : rest) = (diffMin b a) + countSleep rest
countSleep (_ : rest) = countSleep rest

partI :: IO ()
partI = do
    parseResult <- parseInputIO "data/input4.txt"
    case parseResult of
        Left e -> print e >> fail "parser error"
        Right entries -> mapM_ (putStrLn . show ) $ ( splitByGuard . sort) entries


test :: IO ()
test = do
    let e1  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 1, localTimeOfDay= TimeOfDay 0 0 0 })  (BeginShift 10)
        e2  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 1, localTimeOfDay= TimeOfDay 0 5 0 })  FallsAsleep
        e3  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 1, localTimeOfDay= TimeOfDay 0 25 0 })  WakesUp
        e4  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 1, localTimeOfDay= TimeOfDay 0 30 0 })  FallsAsleep
        e5  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 1, localTimeOfDay= TimeOfDay 0 55 0 })  WakesUp
        e6  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 1, localTimeOfDay= TimeOfDay 23 58 0 })  (BeginShift 99 )
        e7  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 2, localTimeOfDay= TimeOfDay 0 44 0 }) FallsAsleep
        e8  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 2, localTimeOfDay= TimeOfDay 0 50 0 }) WakesUp
        e9  = MKEntry (LocalTime { localDay = fromGregorian 1518 11 3, localTimeOfDay= TimeOfDay 0 5 0 }) (BeginShift 10)
        e10 = MKEntry (LocalTime { localDay = fromGregorian 1518 11 3, localTimeOfDay= TimeOfDay 0 24 0 }) FallsAsleep
        e11 = MKEntry (LocalTime { localDay = fromGregorian 1518 11 3, localTimeOfDay= TimeOfDay 0 29 0 }) WakesUp
        e12 = MKEntry (LocalTime { localDay = fromGregorian 1518 11 4, localTimeOfDay= TimeOfDay 0 2 0 }) (BeginShift 99)
        e13 = MKEntry (LocalTime { localDay = fromGregorian 1518 11 4, localTimeOfDay= TimeOfDay 0 36 0 }) FallsAsleep
        e14 = MKEntry (LocalTime { localDay = fromGregorian 1518 11 4, localTimeOfDay= TimeOfDay 0 46 0 }) WakesUp
        e15 = MKEntry (LocalTime { localDay = fromGregorian 1518 11 5, localTimeOfDay= TimeOfDay 0 3 0 }) (BeginShift 99)
        e16 = MKEntry (LocalTime { localDay = fromGregorian 1518 11 5, localTimeOfDay= TimeOfDay 0 45 0 }) FallsAsleep
        e17 = MKEntry (LocalTime { localDay = fromGregorian 1518 11 5, localTimeOfDay= TimeOfDay 0 55 0 }) WakesUp
    print $ ( splitByGuard . sort) [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17]
    print $ ( countSleep . (flip (!!) 0) . splitByGuard . sort) [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17]
