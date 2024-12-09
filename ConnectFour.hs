module ConnectFour where

import System.Environment
import System.Exit
import Data.Maybe
import Data.List
import Data.Char

data Player = Red | Black deriving (Eq, Show)
type Coordinate = (Row, Column) 

type Row = Int
type Column = Int

type Token = (Coordinate, Player)
type Grid = [Token]
type Game = (Grid, Player) -- Current grid and whose turn it is
type Move = Column

data Win = Winner Player | Ongoing | Tie deriving (Show, Eq)

-- STORY 2

winState :: Token -> Grid -> Win
winState ((r,c),pl) grid = 
   let dirs = [[(r+i,c) | i <- [-3..3]], [(r,c+i) | i <- [-3..3]], [(r+i,c+i) | i <- [-3..3]], [(r+i,c-i) | i <- [-3..3]]] --list of list of coordinates
       checkGrid = map (map (\coord -> lookup coord grid)) dirs --list of list of maybe players
       checkWin = map checkFour checkGrid --list of Maybe Players
   in if ((Just pl) `elem` checkWin) then Winner pl else Ongoing --later check for tie

checkFour :: [Maybe Player] -> Maybe Player
checkFour [] = Nothing
checkFour (Just Red:Just Red:Just Red:Just Red:rest) = Just Red
checkFour (Just Black:Just Black:Just Black:Just Black:rest) = Just Black
checkFour (x:rest) = checkFour rest

--story 2 testing
list1, list2, list3, list4 :: Grid
input1, input2, input3, input4 :: Token
list1 = [((1, 2), Red), ((1, 3), Red), ((1, 4), Red), ((1, 5), Red), ((2, 5), Red)]
input1 = ((1, 5), Red)
input2 = ((2, 5), Red)
list2 = [((1, 2), Red), ((2, 2), Red), ((3, 2), Red), ((4, 2), Red)] --returns Red with input 3
input3 = ((4, 2), Red)
list3 = [((1, 2), Red), ((1, 3), Black), ((1, 4), Red), ((1, 5), Red)] --returns Ongoing with input1
list4 = [((1, 1), Red), ((2, 2), Red), ((3, 3), Red), ((4, 4), Red)] --returns Red with input4
input4 = ((4, 4), Red)

-- STORY 3
nextPlayer :: Player -> Player
nextPlayer Red = Black
nextPlayer Black = Red

updateBoard :: Game -> Move -> Game
updateBoard (grid, currentPlayer) col = 
    let aux grid col row = 
            case lookup (row, col) grid of
                Just player -> aux grid col (row+1)
                Nothing -> 
                    let newToken = ((row, col), currentPlayer)
                        newGrid = newToken : grid
                    in (newGrid, nextPlayer currentPlayer)
    in aux grid col 1

makeMove :: Game -> Move -> Game
makeMove game move = if move `elem` legalMoves game then updateBoard game move else game

--STORY 4
-- takes a grid and a column number and returns
-- true if that column is not full of tokens
isNotEmpty :: Grid -> Int -> Bool
isNotEmpty grid column = isNothing $ lookup (6, column) grid

legalMoves :: Game -> [Move]
legalMoves (grid, player) = [col | col <- [1..7], isNotEmpty grid col]

-- Story 4 Test Cases
-- legalMoves (fullGrid, _) == []
-- legalMoves (emptyGrid, _) == [1, 2, 3, 4, 5, 6, 7]
-- legalMoves (fullColGrid, _) = [2, 3, 4, 5, 6, 7]
-- legalMoves (oneMoveLeft, _) = [7]
-- legalMoves (fourMovesLeft, _) = [4, 5]

-- STORY 5

prettyPrint :: Grid -> String
prettyPrint grid = unlines [ prettyRow r | r <- reverse [1..6]]
  where
    prettyRow :: Row -> String
    prettyRow r = unwords [ prettyCell r c | c <- [1..7]]
    
    prettyCell :: Row -> Column -> String
    prettyCell r c = case lookup (r, c) grid of
      Just Red   -> "o"
      Just Black -> "x"
      Nothing    -> "."

-- SPRINT 2

-- Story 8

winTie :: [Move] -> Grid -> Win
winTie legalmoves grid = if (length legalmoves) == 0 then checkAllforWin grid else Ongoing

checkAllforWin :: Grid -> Win
checkAllforWin [] = Tie
checkAllforWin (token:rest) = if winner==Ongoing then checkAllforWin rest else winner
   where winner = winState token rest

-- STORY 9
-- Considers every valid move, the resulting game state, and chooses the move with the 
-- best outcome for the current player. This will involve recursively searching through 
-- the game states that result from that move. Think Scavenge!
whoWillWin :: Game -> Win
whoWillWin (grid, pl) = undefined

chooseMove :: Game -> Maybe Move
chooseMove game = if (length moves)>0 then Just (snd (maximum distanceToWin)) else Nothing
   where moves = legalMoves game
         distanceToWin = [ (moveValue x game, x) | x <- moves ]

moveValue :: Move -> Game -> Int
moveValue move game = value
   where newGameState = makeMove game move
         moves = legalMoves newGameState
         possibleWinner = winState (head (fst newGameState)) (fst newGameState)
         value = if possibleWinner==Ongoing then
                 (if (length moves)==0 then (-1) else (if aux>0 then aux+1 else aux))
                 else 0
         aux = maximum [ moveValue x newGameState | x <- moves ]


-- STORY 10
-- Given a game state, you should  return a move that can force a win for the current 
-- player. Failing that, return a move that can force a tie for the current player.
-- This is very similar to whoWillWin, but keeps track of what the first move was that 
-- could force that outcome. That means you should not use this function to write whoWillWin.
bestMove :: Game -> Move
bestMove (grid, player) = 
    let aux [m] = m
        aux (m:ms) = 
            let who = whoWillWin (makeMove (grid, player) m)
            in case who of
                Winner p -> if p == player then m else aux ms
                Tie -> aux ms
    in aux (legalMoves (grid, player))

-- Story 10 Test Cases
-- bestMove (oneMoveLeft, Red) == 7
-- bestMove (twoMovesLeft, Red) == 4 or 5
-- bestMove (twoMovesLeft, Black) == 4
-- bestMove (fourMovesLeft, Red) == 4
-- bestMove (fourMovesLeft, Black) == 5


-- STORY 11
-- Done for stories 12 and 13

-- STORY 12
-- Takes a string in your text format and returns the corresponding game
readGame :: String -> Game
readGame str =
    let lines' = lines str
        tokens = mapMaybe parseToken (init lines')
        currentPlayer = parsePlayer (drop 5 (last lines'))
    in (tokens, currentPlayer)
  where
    parseToken :: String -> Maybe Token
    parseToken line =
      case span (/= ':') line of
        (coords, ':':player) ->
          let (r, ',':c) = span (/= ',') coords
          in Just ((read r, read c), parsePlayer player)
        _ -> Nothing

    parsePlayer :: String -> Player
    parsePlayer "R" = Red
    parsePlayer "B" = Black
    parsePlayer _ = error "Invalid player symbol"

-- STORY 13
-- Takes a game and turns it into a string in your text format
printGame :: Game -> String
printGame (grid, currentPlayer) =
    let tokens = unlines $ map showToken grid
        turn = "Turn:" ++ showPlayer currentPlayer
    in tokens ++ turn
  where
    showToken :: Token -> String
    showToken ((r, c), player) = show r ++ "," ++ show c ++ ":" ++ showPlayer player

    showPlayer :: Player -> String
    showPlayer Red = "R"
    showPlayer Black = "B"


-- STORY 20 
-- Create at least four more test input files. These should be games that are further from the end, but at an interesting state. 
-- Include games where one player is dominating, games that are evenly matched, and games near the start.

testGame1 :: String
testGame1 = unlines [
    "1,1:R",
    "2,2:R",
    "3,3:R",
    "Turn:B"
  ]

-- A game where one player dominates
testGame2 :: String
testGame2 = unlines [
    "1,1:R",
    "1,2:R",
    "1,3:R",
    "1,4:R",
    "Turn:B"
  ]

-- An evenly matched game
testGame3 :: String
testGame3 = unlines [
    "1,1:R",
    "1,2:B",
    "2,1:R",
    "2,2:B",
    "Turn:R"
  ]

-- A near-start game
testGame4 :: String
testGame4 = unlines [
    "Turn:R"
  ]

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
    exitSuccess

-- STORY 25: Support the -m <move>, --move <move> flag, which should <move> and print out the resulting board to stdout.
-- You should print in the input format. If the -v flag is provided, you may pretty-print instead.
-- The move should be 1-indexed. If a move requires multiple values, the move should be a tuple of numbers separated by a comma with no space. You may use letters instead of numbers if you choose, which should be lower-cased and 'a'-indexed.
-- Estimate: 1, for full credit.

handleMove :: Game -> Move -> Bool -> IO ()
handleMove game move verbose = do
    let newGame = makeMove game move
    if verbose
        then putStrLn $ prettyPrint (fst newGame)
        else putStrLn $ printGame newGame



-- Story 17 - Edited in Story 19
type Rating = Int
rateGame :: Game -> Rating
rateGame (grid, pl) = if (possibleWinner/=Ongoing) then (if possibleWinner==Red) then 1000 else (-1000)) else
   let rows = [ [ (r,c) | c <- [1..7] ] | r <- [1..6] ]
   	   cols = [ [ (r,c) | r <- [1..6] ] | c <- [1..7] ]
     	 positive = nub $ [ [(x+i,y+i) | i <- [0..5] , x+i<=6] | (x,y) <- [ (r,1) | r <- [1..3] ] ] ++ [ [(x+i,y+i) | i <- [0..5], y+i<=7 ] | (x,y) <- [ (1, c) | c <- [2..4] ] ]
       negative = nub $ [ [(x-i,y+i) | i <- [0..5], x-i>=1 ] | (x,y) <- [ (r,1) | r <- [4..6] ] ] ++ [ [(x-i,y+i) | i <- [0..5], y+i<=7 ] | (x,y) <- [ (6, c) | c <- [2..4] ] ]
       all = rows ++ cols ++ positive ++ negative
   in (rateFour all grid)
   where possibleWinner = winState (head grid) (grid, pl)
    	 

rateFour :: [[(Int,Int)]] -> Grid -> Rating
rateFour coords grid = (aux 0 Red colors) - (aux 0 Black colors)
   where colors = map (map (\coord -> lookup coord grid)) coords
     	   aux n [] = n
     	   aux n pl (x:xs) = aux (count (change pl x) n) pl xs
     	   change [] = []
     	   change pl (y:ys) = ((y == Just pl)||(isNothing y)):(change ys)
     	   count [] n = n
     	   count (True:True:True:True:zs) n = count zs (n+1)
     	   count (x:rest) n = count rest n




-- Story 18 - Edited in Story 19

{-whoMightWin :: Game -> Int -> Maybe (Rating, Move)
whoMightWin game cutOff = if (length moves)=0 then Nothing else
   let moves = legalMoves game
       values = [ (moveValue x game cutOff, x) | x <- moves ]
       min = minimum values
       max = maximum values
       checker = if (fst min)<0 then (snd min) else (if (fst max)>0 then (snd max) else Nothing)
   in if (isNothing checker) then Nothing else ((rateGame (makeMove game checker)), checker)


moveValueCutOff :: Move -> Game -> Int -> Int
moveValueCutOff move (grid, pl) cutOff = value
   where newGameState = makeMove (grid, pl) move
         moves = legalMoves newGameState
         possibleWinner = winState (head (fst newGameState)) (fst newGameState)
         value = if possibleWinner==Ongoing then
                    (if cutOff==0 then 
                        (let rater = (rateGame newGameState) in (if rater>0 then rater else 0))
                    else checker)
                 else (if possibleWinner==pl then (-1000) else 0)
         values = [ moveValueCutOff x newGameState (cutOff-1) | x <- moves ]
         min = minimum values
         max = maximum values
         checker = if min<0 then min+1 else (if max>0 then max else 0)-}


whoMightWin :: Game -> Int -> Maybe (Rating, Move)
whoMightWin (grid, pl) cutOff = aux moves (grid, pl) cutOff
   where moves = legalMoves (grid, pl)
         winCondition = if pl==Red then 1000 else (-1000)
         loseCondition = (-(winCondition))
         f x y = if pl==Red then (max x y) else (min x y)
         aux [] _ _ = Nothing
         aux [m] gme 0 = Just (rateGame (makeMove gme m), m)
         aux (m:ms) gme 0 = let newGameState = makeMove gme m
                                rate = rateGame newGameState
                            in if (rate==winCondition)||((length newMoves)==0) then (Just (rate, m)) else f (Just (rate, m)) (aux ms gme 0)
         aux [m] gme cutOff = let newGameState = makeMove gme m
                                  newMoves = legalMoves newGameState
                                  rate = rateGame newGameState
                              in if (rate==1000)||(rate==(-1000))||((length newMoves)==0) then (Just (rate, m)) else (aux newMoves newGameState (cutOff-1))
         aux (m:ms) gme cutOff = let newGameState = makeMove gme m
                                     newMoves = legalMoves newGameState
                                     rate = rateGame newGameState
                                 in if (rate==winCondition)||((length newMoves)==0) then (Just (rate, m)) else 
                                    (if rate==loseCondition then aux ms gme cutOff else f (aux ms gme cutOff) (aux newMoves newGameState (cutOff-1)))
