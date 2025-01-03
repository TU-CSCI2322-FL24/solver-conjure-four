module Main where

import System.IO
import System.Environment
import ConnectFour
import TestGrids
import Data.Maybe
import System.Console.GetOpt
import Data.List
import Control.Monad (when)

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
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    if Help `elem` flags
    then printHelp
    else if Interactive `elem` flags
         then interactiveMode ([], Red) (Verbose `elem` flags) (getNumber flags)
         else do
             let flpath = if null inputs 
                          then error "Please provide a filepath." 
                          else head inputs
             fileStr <- readFile flpath
             let game = readGame fileStr
             dispatch flags game


-- Add more cases here to cover more flags
dispatch :: [Flag] -> Game -> IO ()
dispatch flags game
    | Interactive `elem` flags = do
        let cutoff = getNumber flags -- Default depth or specified depth
        let verbose = Verbose `elem` flags
        interactiveMode game verbose cutoff
    | any isNumber flags       = putBestMoveDepth game (getNumber flags) (Verbose `elem` flags)
    | WinResult `elem` flags   = putBestMove game (Verbose `elem` flags)
    | any isMove flags         = handleMove game (getMove flags) (Verbose `elem` flags)
    | otherwise                = putBestMoveDepth game 8 False -- STORY 21


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



-- STORY 14

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
putBestMove :: Game -> Bool -> IO ()
putBestMove game verbose =
    do  let move = bestMove game
        putStrLn $ "The best move for this game is column " ++ show move
        let outcome = whoWillWin game
        when verbose $ putStrLn ("This move will force the following outcome: " ++ (showOutcome outcome))



-- STORY 23 Helper Functions

putBestMoveDepth :: Game -> Int -> Bool -> IO ()
putBestMoveDepth game cutoff verbose =
    do  let bestM = goodMove game cutoff
        let rating = whoMightWin game cutoff
        putStrLn $ "The best move for this game is column " ++ show bestM
        when verbose $ putStrLn ("This move has a rating of " ++ show rating)



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



-- STORY 25

-- Support the -m <move>, --move <move> flag, which should <move> and print out the resulting board to stdout.
-- You should print in the input format. If the -v flag is provided, you may pretty-print instead.
-- The move should be 1-indexed. If a move requires multiple values, the move should be a tuple of numbers separated by a comma with no space. You may use letters instead of numbers if you choose, which should be lower-cased and 'a'-indexed.
-- Estimate: 1, for full credit.
handleMove :: Game -> Move -> Bool -> IO ()
handleMove game move verbose = do
    let newGame = makeMove game move
    if verbose
        then putStrLn $ prettyPrint (fst newGame)
        else putStrLn $ showGame newGame

-- for testing
printGrid :: Grid -> IO ()
printGrid grid =
    do let str = prettyPrint grid
       putStrLn str




-- Interactive gameplay
interactiveMode :: Game -> Bool -> Int -> IO ()
interactiveMode game@(grid, currentPlayer) verbose cutoff = do
    -- Check if the grid is empty
    if null grid
    then playTurn game verbose cutoff
    else case winState (head grid) grid of
        Just (Winner player) -> putStrLn $ "Player " ++ show player ++ " wins!"
        Just Tie             -> putStrLn "The game ends in a tie."
        Nothing              -> playTurn game verbose cutoff

playTurn :: Game -> Bool -> Int -> IO ()
playTurn game@(grid, currentPlayer) verbose cutoff = 
    if currentPlayer == Red
    then do
        putStrLn "Your turn! Enter a column number (1-7):"
        moveStr <- getLine
        case reads moveStr :: [(Int, String)] of
            [(move, "")] | move `elem` legalMoves game -> do
                let newGame = makeMove game move
                if verbose
                then putStrLn $ prettyPrint (fst newGame)
                else putStrLn $ showGame newGame
                interactiveMode newGame verbose cutoff
            _ -> do
                putStrLn "Invalid move. Please try again."
                playTurn game verbose cutoff
    else do
        let move = goodMove game cutoff
        putStrLn $ "Computer chooses column: " ++ show move
        let newGame = makeMove game move
        if verbose
        then putStrLn $ prettyPrint (fst newGame)
        else putStrLn $ showGame newGame
        interactiveMode newGame verbose cutoff
