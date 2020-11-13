import System.Random
import System.Exit
import Control.Monad 
import Data.Char
import Data.Maybe
import GameState

hangmanWords :: IO [String]
hangmanWords = words <$> readFile "words.txt"

main :: IO ()
main = do
    newGame

gameLoop :: GameState -> IO ()
gameLoop state = do
    when (gameLost state) $ do
        restart <- promptRestart ("You ran out of guesses!" ++ "\nThe Word was " ++ word state)
        if restart then newGame 
        else exitSuccess

    when (gameWon state) $ do
        restart <- promptRestart "Word guessed!"
        if not restart then exitSuccess
        else newGame

    printState state
    (c:_) <- map toLower <$> getLine
    if c `notElem` guessed state then
        gameLoop $ state {guessed = c:guessed state}
    else
        gameLoop state

newGame :: IO ()
newGame = do
    gen <- newStdGen
    words <- hangmanWords
    let word = getRandomWord gen words
    gameLoop (GameState word [] 5)

getRandomWord :: StdGen -> [String] -> String
getRandomWord gen words = do
    let index = fst $ randomR (0, length words -1) gen
    map toLower (words !! index)

promptRestart :: String -> IO Bool
promptRestart msg = do
    putStrLn (msg ++ "\nPress y to restart or anything else to quit")
    (c:_) <- map toLower <$> getLine
    return $ validateRestartPrompt c

validateRestartPrompt :: Char -> Bool
validateRestartPrompt c = not (c `notElem` "yn") && c=='y'