module TestGrids where

import ConnectFour

-- TEST CASES (feel free to add more)
emptyGrid, fullColGrid, fullGrid, oneMoveLeft, twoMovesLeft, fourMovesLeft :: Grid
emptyGrid = []
fullColGrid = [((1, 1), Red), ((2, 1), Black), ((3, 1), Red), ((4, 1), Black), ((5, 1), Red), ((6, 1), Black)]
fullGrid = [((row, col), Red) | row <- [1..6], col <- [1..7]]
oneMoveLeft = init fullGrid -- Every space filled but (6, 7) (top right)

-- Up to two moves left in the game.
-- Red wins with either move if they play first.
-- If Black moves first, they win on column 4 and lose on column 5.
twoMovesLeft = fourMovesLeft ++ [((5, 4), Red), ((5, 5), Black)]

-- Up to four moves left in the game. 
-- Red forces a win if they play column 4 first. 
-- Black can force a Tie if they move first.
fourMovesLeft = [((row, col), Red) | row <- [1..3], col <- [2, 6]] ++ [((row, col), Red) | row <- [4..6], col <- [1, 7]]
    ++ [((row, col), Black) | row <- [1..3], col <- [1, 3, 7]] ++ [((row, col), Black) | row <- [4..6], col <- [2, 6]]
    ++ [((1, 4), Black), ((2, 4), Black), ((3, 4), Red), ((4, 4), Red), ((1, 5), Black), ((2, 5), Black), ((3, 5), Black), ((4, 5), Red)
    , ((4, 3), Red), ((5, 3), Red), ((6, 3), Black)]