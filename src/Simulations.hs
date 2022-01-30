module Simulations (simulate) where

import Types
import System.Random


simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m robots kids obstacles dirt seed = do 
    putStrLn "FIN"
    putStrLn "---------------------------------------------------------------------------------------"
    



generateEmptyBoard :: Int -> Int -> Board
generate_board n m = generateRows n m 0


generateRows :: Int -> Int -> Int -> Board
generateRows rows columns row
    | row == rows = []
    | otherwise = [generateColumns rows columns row 0 : generateRows rows columns (row+1)] 

generateColumns :: Int -> Int -> Int -> [BoardCell]
generateColumns rows columns row column 
    | column == columns = []
    | otherwise = [(Empty, (row, column)) : generateColumns rows columns row (column+1)]

-- generate_board :: Int -> Int -> Int -> Int -> Int -> Int -> Board
-- generate_board n m robots kids obstacles dirt = board where
--     board = generate_robots board n m robots
--     board = generate_kids board n m kids

-- generate_robots :: Board -> Int -> Int -> Int -> Board
-- generate_robots board n m count = board where 
