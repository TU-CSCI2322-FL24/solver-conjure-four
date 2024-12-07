module Main where

import System.IO
import System.Environment
import ConnectFour
import TestGrids
import Data.Maybe
import System.Console.GetOpt

data Flag = WinResult | Number String | Help | MoveNum String | Verbose | Interactive deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg WinResult) "a."
          , Option ['d'] ["depth"] (ReqArg Number "<num>") "a."
          , Option ['h'] ["help"] (NoArg Help) "a."
          , Option ['m'] ["move"] (ReqArg MoveNum "<num>") "a."
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
        putStrLn $ "The best move for this game is column " ++ show move
        let outcome = whoWillWin game
        putStrLn $ "This move will force the following outcome: " ++ (showOutcome outcome)

putBestMoveDepth :: Game -> Int -> IO ()
putBestMoveDepth game cutOff = 
    case whoMightWin game cutOff of
        Nothing -> putStrLn "whoMightWin returned Nothing"
        Just (rating, move) -> 
            do putStrLn $ "The best move for this game is column " ++ show move

showOutcome :: Win -> String
showOutcome (Winner pl) = "Player " ++ (show pl) ++ " wins"
showOutcome Tie = "Tie"
showOutcome _ = "whoWillWin returned Ongoing or an invalid Win state"

-- Reads a file name from the arguments, behaves based on given flag
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
                dispatch flags game

-- Add more cases here to cover more flags
-- otherwise case is for Story 21 I think
dispatch :: [Flag] -> Game -> IO ()
dispatch flags game
    | any isNumber flags     = putBestMoveDepth game (getNumber flags)
    | WinResult `elem` flags = putBestMove game
    | otherwise = undefined

isNumber :: Flag -> Bool
isNumber (Number _) = True
isNumber _ = False

getNumber :: [Flag] -> Int
getNumber [] = 1
getNumber (Number x:_) = read x
getNumber (_:flags) = getNumber flags

-- start changing here, detect which flag was given. if -w, call putBestMove game, if -d num, call putBestMoveDepth game num