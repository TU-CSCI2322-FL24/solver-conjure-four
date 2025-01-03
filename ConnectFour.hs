module ConnectFour where

import Data.Maybe
import Data.List (nub)

data Player = Red | Black deriving (Eq, Show)
type Coordinate = (Row, Column) 

type Row = Int
type Column = Int

type Token = (Coordinate, Player)
type Grid = [Token]
type Game = (Grid, Player) -- Current grid and whose turn it is
type Move = Column

data Win = Winner Player | Tie deriving (Show, Eq)

type Rating = Int

-- STORY 2

winState :: Token -> Grid -> Maybe Win
winState ((r,c),pl) grid = 
   let dirs = [[(r+i,c) | i <- [-3..3]], [(r,c+i) | i <- [-3..3]], [(r+i,c+i) | i <- [-3..3]], [(r+i,c-i) | i <- [-3..3]]] --list of list of coordinates
       checkGrid = map (map (\coord -> lookup coord grid)) dirs --list of list of maybe players
       checkWin = map checkFour checkGrid --list of Maybe Players
       moves = legalMoves (grid, pl)
   in if ((Just pl) `elem` checkWin) then Just (Winner pl) else (if null moves then Just Tie else Nothing) --later check for tie

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

-- STORY 9

-- Considers every valid move, the resulting game state, and chooses the move with the 
-- best outcome for the current player. This will involve recursively searching through 
-- the game states that result from that move. Think Scavenge!
whoWillWin :: Game -> Win
whoWillWin (grid, pl) = 
    let state = winState (head grid) grid
        moves = legalMoves (grid, pl)
        possibleStates = [whoWillWin resultGame | m <- moves, let resultGame = makeMove (grid, pl) m]
    in case state of
        Nothing -> getBestWin possibleStates pl
        Just endState -> endState

-- Given a list of win states, returns the best one for the given player
-- Winner pl > Tie > Winner not pl
getBestWin :: [Win] -> Player -> Win
getBestWin [w] pl = w
getBestWin (w:ws) pl = 
    let currBest = getBestWin ws pl
    in if compareWin w currBest pl then w else currBest

-- Returns true if win state 1 is better than or equal for the given player than win state 2
compareWin :: Win -> Win -> Player -> Bool
compareWin (Winner p) _ pl = p == pl
compareWin _ (Winner p) pl = p /= pl
compareWin (Tie) (Tie) pl = True



-- STORY 10

-- Given a game state, you should  return a move that can force a win for the current 
-- player. Failing that, return a move that can force a tie for the current player.
-- This is very similar to whoWillWin, but keeps track of what the first move was that 
-- could force that outcome. That means you should not use this function to write whoWillWin.
bestMove :: Game -> Move
bestMove (grid, player) = 
    let aux [m] = (m, whoWillWin (makeMove (grid, player) m))
        aux (m:ms) = 
            let who = whoWillWin (makeMove (grid, player) m)
                (bestM, bestWin) = aux ms
            in if compareWin who bestWin player then (m, who) else (bestM, bestWin)
    in fst $ aux (legalMoves (grid, player))

-- Story 10 Test Cases
-- bestMove (oneMoveLeft, Red) == 4
-- bestMove (twoMovesLeft, Red) == 4 or 5
-- bestMove (twoMovesLeft, Black) == 4
-- bestMove (fourMovesLeft, Red) == 4 or 5 (assuming Black is playing the best move, Red can't win)
-- bestMove (fourMovesLeft, Black) == 4



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
showGame :: Game -> String
showGame (grid, currentPlayer) =
    let tokens = unlines $ map showToken grid
        turn = "Turn:" ++ showPlayer currentPlayer
    in tokens ++ turn
  where
    showToken :: Token -> String
    showToken ((r, c), player) = show r ++ "," ++ show c ++ ":" ++ showPlayer player

    showPlayer :: Player -> String
    showPlayer Red = "R"
    showPlayer Black = "B"



-- STORY 17

rateGame :: Game -> Rating
rateGame (grid, pl) = 
    let state = winState (head grid) grid
    in case state of
        Just (Winner Red) -> 500
        Just (Winner Black) -> -500
        Just Tie -> 0
        Nothing -> rateOngoing (grid, pl)

-- aux checks how many tokens have an adjacent token of the same color
rateOngoing :: Game -> Rating
rateOngoing (grid, pl) = 
    let redScore = aux grid Red
        blackScore = aux grid Black
        aux [] pl = 0
        aux (((r, c), tokPl):toks) pl = 
            let adjacents = catMaybes [ lookup (r+1, c-1) grid, lookup (r+1, c) grid, lookup (r+1, c+1) grid,
                                        lookup (r, c-1) grid,                         lookup (r, c+1) grid,
                                        lookup (r-1, c-1) grid, lookup (r-1, c) grid, lookup (r-1, c+1) grid ]
            in if (tokPl == pl) && (pl `elem` adjacents) then (aux toks pl) + 1 else aux toks pl
    in redScore - blackScore



-- STORY 18

whoMightWin :: Game -> Int -> Rating
whoMightWin (grid, pl) 0 = rateGame (grid, pl)
whoMightWin (grid, pl) cutoff = 
    let state = winState (head grid) grid
        moves = legalMoves (grid, pl)
        ratings = [whoMightWin resultGame (cutoff-1) | m <- moves, let resultGame = makeMove (grid, pl) m]
    in case state of
        Nothing -> if pl == Red then maximum ratings else minimum ratings
        Just endState -> rateGame (grid, pl)

goodMove :: Game -> Int -> Move
goodMove (grid, pl) cutoff = 
    let aux [m] = (m, whoMightWin (makeMove (grid, pl) m) cutoff)
        aux (m:ms) = 
            let who = whoMightWin (makeMove (grid, pl) m) cutoff
                (bestM, bestRating) = aux ms
            in if (pl == Red && who >= bestRating) || (pl == Black && who <= bestRating) then (m, who) else (bestM, bestRating)
    in fst $ aux (legalMoves (grid, pl))

