data Player = Red | Black
type Coordinate = (Int, Int)

type Token = (Coordinate, Player)
type Grid = [Token]
type Game = (Grid, Player) -- Current grid and whose turn it is
type Move = (Player, Int)

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
-- true if that column is full of tokens
isFull :: Grid -> Int -> Bool
isFull grid column = 
    let numTokens = length [col | ((row, col), p) <- grid, col == column]
    in numTokens >= 6

-- Takes a game state (a grid and a player)
-- and returns the list of legal moves on this grid
-- by this player.
legalMoves :: Game -> [Move]
legalMoves (grid, player) = [(player, col) | col <- [1..7], not (isFull grid col)]

-- STORY 5
prettyPrint :: Grid -> String
prettyPrint = undefined