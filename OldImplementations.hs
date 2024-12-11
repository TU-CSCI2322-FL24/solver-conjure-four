-- These functions were removed and rewritten

-- whoMightWin First Version

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



-- whoMightWin Second Version

-- whoMightWin (grid, pl) cutOff = aux moves (grid, pl) cutOff
--    where moves = legalMoves (grid, pl)
--          winCondition = if pl==Red then 1000 else (-1000)
--          loseCondition = (-(winCondition))
--          f x y = if pl==Red then (max x y) else (min x y)
--          aux [] _ _ = Nothing
--          aux [m] gme 0 = Just (rateGame (makeMove gme m), m)
--          aux (m:ms) gme 0 = let newGameState = makeMove gme m
--                                 rate = rateGame newGameState
--                             in if (rate==winCondition)||((length newMoves)==0) then (Just (rate, m)) else f (Just (rate, m)) (aux ms gme 0)
--          aux [m] gme cutOff = let newGameState = makeMove gme m
--                                   newMoves = legalMoves newGameState
--                                   rate = rateGame newGameState
--                               in if (rate==1000)||(rate==(-1000))||((length newMoves)==0) then (Just (rate, m)) else (aux newMoves newGameState (cutOff-1))
--          aux (m:ms) gme cutOff = let newGameState = makeMove gme m
--                                      newMoves = legalMoves newGameState
--                                      rate = rateGame newGameState
--                                  in if (rate==winCondition)||((length newMoves)==0) then (Just (rate, m)) else 
--                                     (if rate==loseCondition then aux ms gme cutOff else f (aux ms gme cutOff) (aux newMoves newGameState (cutOff-1)))



-- whoWillWin Second Version

-- chooseMove :: Game -> Maybe Move
-- chooseMove game = if (length moves)>0 then Just (snd (maximum distanceToWin)) else Nothing
--    where moves = legalMoves game
--          distanceToWin = [ (moveValue x game, x) | x <- moves ]

-- moveValue :: Move -> Game -> Int
-- moveValue move game = value
--    where (newGrid, newPl) = makeMove game move
--          moves = legalMoves (newGrid, newPl)
--          possibleWinner = winState (head newGrid) (newGrid)
--          value = if possibleWinner==Ongoing then
--                  (if (length moves)==0 then (-1) else (if aux>0 then aux+1 else aux))
--                  else 0
--          aux = maximum [ moveValue x (newGrid, newPl) | x <- moves ]



-- rateGame First Version

-- rateGame (grid, pl) = if (possibleWinner/=Ongoing) then (if possibleWinner==Red) then 1000 else (-1000)) else
--    let rows = [ [ (r,c) | c <- [1..7] ] | r <- [1..6] ]
--    	   cols = [ [ (r,c) | r <- [1..6] ] | c <- [1..7] ]
--      	 positive = nub $ [ [(x+i,y+i) | i <- [0..5] , x+i<=6] | (x,y) <- [ (r,1) | r <- [1..3] ] ] ++ [ [(x+i,y+i) | i <- [0..5], y+i<=7 ] | (x,y) <- [ (1, c) | c <- [2..4] ] ]
--        negative = nub $ [ [(x-i,y+i) | i <- [0..5], x-i>=1 ] | (x,y) <- [ (r,1) | r <- [4..6] ] ] ++ [ [(x-i,y+i) | i <- [0..5], y+i<=7 ] | (x,y) <- [ (6, c) | c <- [2..4] ] ]
--        all = rows ++ cols ++ positive ++ negative
--    in (rateFour all grid)
--    where possibleWinner = winState (head grid) (grid, pl)


-- rateFour :: [[(Int,Int)]] -> Grid -> Rating
-- rateFour coords grid = (aux 0 Red colors) - (aux 0 Black colors)
--    where colors = map (map (\coord -> lookup coord grid)) coords
--      	   aux n [] = n
--      	   aux n pl (x:xs) = aux (count (change pl x) n) pl xs
--      	   change [] = []
--      	   change pl (y:ys) = ((y == Just pl)||(isNothing y)):(change ys)
--      	   count [] n = n
--      	   count (True:True:True:True:zs) n = count zs (n+1)
--      	   count (x:rest) n = count rest n