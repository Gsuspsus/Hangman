import System.Random
import System.Exit
import Control.Monad 
import Data.Char
import Data.Maybe
import GameState

main :: IO ()
main = do
    word <- getRandomWord
    gameLoop (GameState word [] 5)

gameLoop :: GameState -> IO ()
gameLoop state = do
    when (gameLost state) $ do
        answer <- promptRestart ("You ran out of guesses!" ++ "\nThe Word was " ++ word state)
        if answer then do 
            newWord <- getRandomWord
            gameLoop (GameState newWord [] 5)
        else 
            exitSuccess

    when (gameWon state) $ do
        answer <- promptRestart "Word guessed!"
        if answer then
            exitSuccess
        else do
            newWord <- getRandomWord
            gameLoop (GameState newWord [] 5)

    printState state
    (c:_) <- map toLower <$> getLine
    if c `notElem` word state && c `notElem` getWrongs state then
        gameLoop $ state {guessed = c:guessed state}
    else
        gameLoop state

getRandomWord :: IO String
getRandomWord = do
    words <- lines <$> readFile "words.txt"
    index <- randomRIO (0, length words -1)

    return $ map toLower (words !! index)

promptRestart :: String -> IO Bool
promptRestart msg = do
    putStrLn (msg ++ "\nPress y to restart or anything else to quit")
    (c:_) <- map toLower <$> getLine
    return $ validateRestartPrompt c

validateRestartPrompt :: Char -> Bool
validateRestartPrompt c = not (c `notElem` "yn") && c=='y'