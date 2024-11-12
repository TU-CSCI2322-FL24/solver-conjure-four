data Player = Red | Black

-- Do something to limit row/column to 6/7?
type Coordinate = (Row, Column)
type Row = Int 
type Column = Int 

type Token = (Coordinate, Player)
type Grid = [Token]
type Game = (Grid, Player) -- Current grid and whose turn it is
type Move = (Player, Column)

data Win = Winner Player | None

-- STORY 2
findWinner :: Token -> [Tokens] -> Win --If the new move won then return the player of the new move as winner, else none
findWinner newMove tokens = if (winState newMove tokens) then (Winner (snd newMove)) else None

winState :: Token -> [Tokens] -> Boolean
winState newMove tokens = checkDirs newMove dirs
   where adjacent _ [] = []
         adjacent ((r1 c1),pl1) (((r2 c2), pl2):ts) = --((row1 collumn1), player1)
            let rdif = r2 - r1
                cdif = c2 - c1
            in if ((pl1==pl2) && ((rdif>=-1)&&(rdif<=1)) && ((cdif>=-1) && (cdif<=1)) && ((cdif/=0) || (rdif/=0)))
						   -- ^ checks if a token is within a distance of 1 horizontally and vertically
               then (r2 c2):(adjacent ((r1 c1),pl1) ts) -- creates a list of all adjacent tokens
               else adjacent ((r1 c1),pl1) ts
         dirs = adjacent newMove tokens -- this is the list of adjacent tokens
				 checkDirs _ [] = False
         checkDirs ((r1 c2), pl) ((r2 c2):ds) = if (checkFour ((r1 c1), pl) (r2 c2)) then True else (checkDirs ((r1 c2), pl) ds)
				 -- ^ checks each direction, returning true if any direction creates a connect four
         checkFour ((r1 c1), pl) (r2 c2) = if (rdif==4 || cdif==4) then True else
                                    (if (lookFor ((a b), pl) tokens) then (checkFour ((r1 c1), pl) (a b)) else False) -- looks for connect four, if looking for a fifth then win
            where rdif = r2 - r1 --row difference
                  cdif = c2 - c1 --column difference
                  a = r1+(newD rdif) --row value of next token in connect four
                  b = c1+(newD cdif) --column value of next token in connect four
                  newD num = if num==0 then 0 else (if num>0 then num+1 else num-1) --finds distance between move and next token in connect four

lookFor :: Coordinate -> [Tokens] -> Boolean
lookFor _ [] = False --Checks if token with certain cordinates exists in a list
lookFor ((r1 c1), pl1) (((r2 c2),p2):ts) = if ((r1==r2) && (c1==c2) && (pl1==pl2)) then True else lookFor ((r1 c1), pl1) ts


--story 2 testing
list1 = [((1 2), Red), ((1 3), Red), ((1 4), Red)] --returns true with input1, false with input2
input1 = ((1 5), Red)
input2 = ((2 5), Red)
list2 = [((1 2), Red), ((2 2), Red), ((3 2), Red)] --returns true with input 3
input3 = ((4 2), Red)
list3 = [((1 2), Red), ((1 3), Black), ((1 4), Red)] --returns false with input1
list4 = [((1 1), Red), ((2 2), Black), ((3 3), Red)] --returns true with input4
input4 = ((4 4), Red)

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
