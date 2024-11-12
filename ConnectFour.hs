import Data.Maybe

data Player = Red | Black deriving (Eq, Show)
type Coordinate = (Row, Column) 

type Row = Int
type Column = Int

type Token = (Coordinate, Player)
type Grid = [Token]
type Game = (Grid, Player) -- Current grid and whose turn it is
type Move = Column

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
-- takes a grid and a column number and returns
-- true if that column is not full of tokens
isNotEmpty :: Grid -> Int -> Bool
isNotEmpty grid column = isNothing $ lookup (6, column) grid

legalMoves :: Game -> [Move]
legalMoves (grid, player) = [col | col <- [1..7], isNotEmpty grid col]

-- STORY 5
prettyPrint :: Grid -> String
prettyPrint = undefined