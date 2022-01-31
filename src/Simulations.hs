module Simulations
    ( simulate
    ) where

import System.Random ()
import Types ( Board, CellType(Empty), Cell )
import Utils (printBoard)


simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m robots kids obstacles dirt seed = do
    let board = generateBoard n m robots kids obstacles dirt seed
    print board
    putStrLn "FIN"
    putStrLn
        "---------------------------------------------------------------------------------------"
    printBoard board



generateBoard :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Board
generateBoard n m robots kids obstacles dirt seed = board where 
    board = generateEmptyBoard n m


generateEmptyBoard :: Int -> Int -> Board
generateEmptyBoard n m = generateEmptyRows n m 0


generateEmptyRows :: Int -> Int -> Int -> Board
generateEmptyRows rows columns row
    | row == rows = []
    | otherwise = generateEmptyColumns columns row 0
    : generateEmptyRows rows columns (row + 1)

generateEmptyColumns :: Int -> Int -> Int -> [Cell]
generateEmptyColumns columns row column
    | column == columns = []
    | otherwise = (Empty, (row, column))
    : generateEmptyColumns columns row (column + 1)



