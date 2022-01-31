module Simulations
    ( simulate
    ) where

import System.Random (mkStdGen)
import Types ( Board, CellType(Empty), Cell )
import Utils (printBoard, randomGen)



simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m robots kids obstacles dirt seed = do
    let board = generateBoard n m robots kids obstacles dirt seed
    print board
    putStrLn "FIN"
    putStrLn
        "---------------------------------------------------------------------------------------"
    printBoard board
    let (a, seed1) = randomGen 0 10 (mkStdGen seed)
    print a
    print seed1

    let (a, seed2) = randomGen 0 10 seed1
    print a
    print seed2





generateBoard :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Board
generateBoard n m robots kids obstacles dirt seed = board where
    board = generateEmptyBoard n m


-- generate Empty Cells
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



-- generate Corrals
generateCorrals :: Board -> Int -> Int -> Board 
generateCorrals board corralsCount seed
    | corralCount == 0 = board
    | length (filterByCellType Corral board) == 0 = 




-- generate Robots

-- generate Obstacles


-- generate Kids 


-- generate Dirt

