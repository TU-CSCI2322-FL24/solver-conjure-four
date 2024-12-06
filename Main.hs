module Main where

import System.IO
import System.Environment
import ConnectFour
import TestGrids
import Data.Maybe
import System.Console.GetOpt

data Flag = WinResult | Depth String | Help | Verbose | Interactive deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg WinResult) "a."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "a."
          , Option ['h'] ["help"] (NoArg Help) "a."
          , Option ['v'] ["verbose"] (NoArg Verbose) "a."
          , Option ['i'] ["interactive"] (NoArg Interactive) "a."
          ]

-- STORY 14
-- Define a series of IO actions to wrap around these functions, as well as bestMove, and 
-- define a main IO action. You will need to build four I/O actions: one each to read and 
-- write game states from a file, one that computes and prints the winning move, and a simple main action.

-- At least some of these actions will need to be in a Main module that imports your other module(s).

writeGame :: Game -> FilePath -> IO ()
writeGame game flpath = 
    do  let gameStr = showGame game
        writeFile flpath gameStr

loadGame :: FilePath -> IO Game
loadGame flpath = 
    do  fileStr <- readFile flpath
        return (readGame fileStr)

-- Computes the best move and prints it to standard output. 
-- For full credit, also print the outcome that moves forces.
putBestMove :: Game -> IO ()
putBestMove game = 
    do  let move = bestMove game
        putStrLn $ "The best move for this game is column " ++ show (bestMove game)
        let outcome = whoWillWin game
        putStrLn $ "This move will force the following outcome: " ++ (showOutcome outcome)

showOutcome :: Win -> String
showOutcome (Winner pl) = "Player " ++ (show pl) ++ " wins"
showOutcome Tie = "Tie"
showOutcome _ = "whoWillWin returned Ongoing or an invalid Win state"

-- Reads a file name from standard input or the arguments, 
-- loads the game, and prints the best move
main :: IO ()
main = 
    do  args <- getArgs
        let (flags, inputs, errors) = getOpt Permute options args
        if null inputs 
        then putStrLn "Please provide a filepath."
        else 
            do  let flpath = head inputs
                fileStr <- readFile flpath
                let game = readGame fileStr
                putBestMove game