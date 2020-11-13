module Difficulty
(
    Difficulty (Easy, Medium, Hard),
    Criteria,
    readDifficulty,
    parseDifficulty,
    getWordCriteria,
    calculateLives
)

where 

import Data.Char
import Data.Maybe

data Difficulty = Easy | Medium | Hard deriving (Show)
type Criteria = (String -> Bool)

readDifficulty :: IO Difficulty
readDifficulty = do
    putStrLn "Pick a difficulty: (E)asy, (M)edium or (H)ard"
    (c:_) <- getLine
    let dif = parseDifficulty c
    if isNothing dif then do
        putStrLn "Invalid difficulty choice!"
        readDifficulty
    else return (fromJust dif)

parseDifficulty :: Char -> Maybe Difficulty
parseDifficulty c 
    | dif == 'e' = Just Easy
    | dif == 'm' = Just Medium
    | dif == 'h' = Just Hard
    | otherwise = Nothing
        where dif = toLower c

getWordCriteria :: Difficulty -> Criteria
getWordCriteria dif =
    case dif of
        Easy -> (\w -> length w < 5)
        Medium -> (\w -> length w > 5 && length w < 10 )
        Hard -> (\w -> length w > 10)

calculateLives :: Difficulty -> String -> Int
calculateLives dif word = 
    case dif of 
        Easy -> length word * 2
        Medium -> length word
        Hard -> length word `div` 2