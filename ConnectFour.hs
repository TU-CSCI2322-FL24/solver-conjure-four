data Player = Red | Black

-- Do something to limit row/column to 6/7?
type Coordinate = (Row, Column)
type Row = Integer 
type Column = Integer

type Token = (Coordinate, Player)
type Grid = [Token]
type Game = (Grid, Player) -- Current grid and whose turn it is
type Move = (Player, Column)

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
move :: Game -> Move -> Game
move = undefined

--STORY 4
-- (Within 6 rows and 7 columns (possible variable numbers))
legalMoves :: Grid -> [Move]
legalMoves = undefined

-- STORY 5
prettyPrint :: Grid -> String
prettyPrint = undefined
