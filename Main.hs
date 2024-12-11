module Main where

import System.IO
import System.Environment
import ConnectFour
import TestGrids
import Data.Maybe
import System.Console.GetOpt
import Data.List

data Flag = WinResult | Number String | Help | MoveNum String | Verbose | Interactive deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg WinResult) "Show the best move"
          , Option ['d'] ["depth"] (ReqArg Number "<num>") "Specify <num> as a cutoff depth"
          , Option ['h'] ["help"] (NoArg Help) "Show this help message"
          , Option ['m'] ["move"] (ReqArg MoveNum "<num>") "Make a move (1-indexed)"
          , Option ['v'] ["verbose"] (NoArg Verbose) "Pretty-print the board"
          , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game and play against the computer"
          ]


-- MAIN SECTION
-- Reads a file name from the arguments, behaves based on given flag
main :: IO ()
main = 
    do  args <- getArgs
        let (flags, inputs, errors) = getOpt Permute options args
        if Help `elem` flags
        then printHelp
        else 
            do  let flpath = if null inputs then error "Please provide a filepath." else head inputs
                fileStr <- readFile flpath
                let game = readGame fileStr
                dispatch flags game

-- Add more cases here to cover more flags
dispatch :: [Flag] -> Game -> IO ()
dispatch flags game
    | any isNumber flags     = putBestMoveDepth game (getNumber flags)
    | WinResult `elem` flags = putBestMove game
    | any isMove flags       = handleMove game (getMove flags) (Verbose `elem` flags)
    | otherwise              = undefined -- STORY 21

-- isNumber and getNumber are for -d <num>
isNumber :: Flag -> Bool
isNumber (Number _) = True
isNumber _ = False

getNumber :: [Flag] -> Int
getNumber [] = 1
getNumber (Number x:_) = read x
getNumber (_:flags) = getNumber flags

-- isMove and getMove are for -m <move>
isMove :: Flag -> Bool
isMove (MoveNum _) = True
isMove _ = False

getMove :: [Flag] -> Int
getMove [] = 1
getMove (MoveNum x:_) = read x
getMove (_:flags) = getMove flags

showOutcome :: Win -> String
showOutcome (Winner pl) = "Player " ++ (show pl) ++ " wins"
showOutcome Tie = "Tie"
showOutcome _ = "whoWillWin returned Ongoing or an invalid Win state"


-- STORY 14 Functions
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


-- STORY 23 Helper Functions
putBestMoveDepth :: Game -> Int -> IO ()
putBestMoveDepth game cutOff = 
    case whoMightWin game cutOff of
        Nothing -> putStrLn "whoMightWin returned Nothing"
        Just (rating, move) -> 
            do putStrLn $ "The best move for this game is column " ++ show move


-- STORY 24
--Support the -h, --help flag, which should print out a good help message and quit the program.

printHelp :: IO ()
printHelp = do
    putStrLn "Connect Four CLI Help"
    putStrLn "Usage: connectfour [options]"
    putStrLn "Options:"
    putStrLn "  -h, --help            Show this help message"
    putStrLn "  -m <move>, --move <move> Make a move (1-indexed)"
    putStrLn "  -v, --verbose         Pretty-print the board"
    putStrLn "  -w, --winner          Show the best move"
    putStrLn "  -d <num>, --depth <num> Specify <num> as a cutoff depth"
    putStrLn "  -i, --interactive     Start a new game and play against the computer" 

-- STORY 25: Support the -m <move>, --move <move> flag, which should <move> and print out the resulting board to stdout.
-- You should print in the input format. If the -v flag is provided, you may pretty-print instead.
-- The move should be 1-indexed. If a move requires multiple values, the move should be a tuple of numbers separated by a comma with no space. You may use letters instead of numbers if you choose, which should be lower-cased and 'a'-indexed.
-- Estimate: 1, for full credit.

handleMove :: Game -> Move -> Bool -> IO ()
handleMove game move verbose = do
    let newGame = makeMove game move
    if verbose
        then putStrLn $ prettyPrint (fst newGame)
        else putStrLn $ showGame newGame