import Data.Maybe

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
list1 = [((1, 2), Red), ((1, 3), Red), ((1, 4), Red), ((1, 5), Red), ((2, 5), Red)]
input1 = ((1, 5), Red)
input2 = ((2, 5), Red)
list2 = [((1, 2), Red), ((2, 2), Red), ((3, 2), Red), ((4, 2), Red)] --returns Red with input 3
input3 = ((4, 2), Red)
list3 = [((1, 2), Red), ((1, 3), Black), ((1, 4), Red), ((1, 5), Red)] --returns Ongoing with input1
list4 = [((1, 1), Red), ((2, 2), Red), ((3, 3), Red), ((4, 4), Red)] --returns Red with input4
input4 = ((4, 4), Red)

-- STORY 3
-- move (Player, Column) = //check for other tokens in column, row is next up, check if legalMove

nextPlayer :: Player -> Player
nextPlayer Red = Black
nextPlayer Black = Red

nextAvailableRow :: Grid -> Column -> Maybe Row
nextAvailableRow grid col = undefined
--   let allRows = [0..5]
--       occupiedRows = [row | ((row c), _) <- grid, c == col]
--       availableRows = filter (`notElem` occupiedRows) allRows
--   in if null availableRows then Nothing else Just (head availableRows)

updateBoard :: Game -> Move -> Game
updateBoard (grid, currentPlayer) (player, col) = undefined
--   case nextAvailableRow grid col of
--     Nothing -> (grid, currentPlayer)
--     Just row -> 
--       let newToken = ((row, col), player)
--           newGrid = newToken : grid
--       in (newGrid, nextPlayer player)

makeMove :: Game -> Move -> Game
makeMove game move = if move `elem` legalMoves game then updateBoard game move else game

--STORY 4
-- takes a grid and a column number and returns
-- true if that column is not full of tokens
isNotEmpty :: Grid -> Int -> Bool
isNotEmpty grid column = isNothing $ lookup (6, column) grid

legalMoves :: Game -> [Move]
legalMoves (grid, player) = [col | col <- [1..7], isNotEmpty grid col]

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
