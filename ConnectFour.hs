data Player = Red | Black
data Coordinate = Row Column

-- Do something to limit row/column to 6/7?
type Row = Int 
type Column = Int 

type Token = (Coordinate, Player)
type Grid = [Token]
type Game = (Grid, Player) -- Current grid and whose turn it is
type Move = (Player, Column)

-- STORY 2
-- winState (t:ts) = //run through each token
-- Maybe a function for each direction instead?
--    where direction = //list of directions of adjacent tokens
--               Four = //check in direction for four tokens
winState :: Game -> Maybe Player
winState = undefined

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
-- (Within 6 rows and 7 columns (possible variable numbers))
legalMoves :: Game -> [Move]
legalMoves = undefined

-- STORY 5
prettyPrint :: Grid -> String
prettyPrint = undefined