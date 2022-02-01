module Utils
    ( printBoard
    , randomGen
    , replaceCell
    , replaceCellList
    ) where

import System.Random ( Random(randomR), StdGen )
import Types ( Board, CellType, Cell )


randomGen :: Int -> Int -> StdGen -> (Int, StdGen)
randomGen min max = randomR (min, max)



-----VISUAL-----
printBoard :: Board -> IO ()
printBoard board = printBoardAux board 0

printBoardAux :: Board -> Int -> IO ()
printBoardAux board i = do
    drawIndex (length (head board))
    drawBoard board 0

drawIndex :: Int -> IO ()
drawIndex n = do
    let index = [0 .. n]
    drawIndexAux index

drawIndexAux :: [Int] -> IO ()
drawIndexAux []      = putStr "\n"
drawIndexAux (h : t) = do
    putStr "   "
    putStr (show h)
    drawIndexAux t

drawBoard :: [[(CellType, (Int, Int))]] -> Int -> IO ()
drawBoard []      _     = putStr "\n"
drawBoard (h : t) count = do
    putStr (show count)
    drawRow h
    drawBoard t (count + 1)

drawRow :: [(CellType, (Int, Int))] -> IO ()
drawRow []                       = putStr "\n"
drawRow ((cellType, (_, _)) : t) = do
    putStr (show cellType)
    drawRow t


replace :: [a] -> Int -> a -> [a]
replace list index element =
    let (first, x : xs) = splitAt index list in first ++ (element : xs)

-- replace a Cell on the Board
replaceCell :: Cell -> Board -> Board
replaceCell (cellType, (row, column)) board =
    replace board row (replace (board !! row) column (cellType, (row, column)))

-- given a list of Cells update the Board
replaceCellList :: [Cell] -> Board -> Board 
replaceCellList [] board = board
replaceCellList ((cellType, (row, column)) : t) board = replaceCellList t (replace board row (replace (board !! row) column (cellType, (row, column))))






-- printBoardAux :: Board -> Int -> Int -> [[Char]]
-- printBoardAux board index rows
--     | index == rows = [""]
--     | otherwise = ["", printRow (getRowByIndex board Int Int board index)] ++ printBoardAux board (index+1) rows

-- printRow :: [[CellType]] -> [Char]
-- printRow [] = ""
-- printRow (cellType : cellTypes) = "" ++ show cellType ++ printRow cellTypes



