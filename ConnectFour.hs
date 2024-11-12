data Player = Red | Black

-- Do something to limit row/column to 6/7?
type Coordinate = (Row, Column)
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
move :: Game -> Move -> Game
move = undefined

--STORY 4
-- (Within 6 rows and 7 columns (possible variable numbers))
legalMoves :: Grid -> [Move]
legalMoves = undefined

-- STORY 5
prettyPrint :: Grid -> String
prettyPrint = undefined
