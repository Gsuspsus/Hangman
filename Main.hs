import System.Random
import System.Exit
import Control.Monad 
import Data.Char
import GameState

main :: IO ()
main = do
    word <- getRandomWord
    gameLoop (GameState word [] 5)

gameLoop :: GameState -> IO ()
gameLoop state = do
    when (remainigGuesses state <= 0) $ do
        putStrLn "You ran out of guesses"
        exitSuccess

    when ((length $ word state) == (length $ getRights state)) $ do
        putStrLn "Word guessed! Play again? (y/n)"
        (c:_) <- map toLower <$> getLine
        if c == 'n' then
            exitSuccess
        else if c == 'y' then do
            word <- getRandomWord
            gameLoop (GameState word [] 5)
        else
            gameLoop state

    printState state
    (c:_) <- getLine
    let misses = if c `notElem` word state then 1 else 0
    gameLoop $ state {guessed = c:guessed state, remainigGuesses = remainigGuesses state - misses}

getRandomWord :: IO String
getRandomWord = do
    words <- lines <$> readFile "words.txt"
    index <- randomRIO (0, length words -1)

    return $ words !! index