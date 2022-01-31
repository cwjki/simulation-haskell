module Utils (drawBoard) where

import System.Random
import Types


randomGen :: Int -> Int -> StdGen -> (Int, StdGen)
randomGen min max = randomR (min, max)


-----VISUAL-----

drawBoard :: [[ (CellType, (Int, Int)) ]] -> IO ()
drawBoard [] = putStr "\n"
drawBoard (h : t) = do
    drawColumn h
    drawBoard t

drawColumn :: [ (CellType, (Int, Int)) ] -> IO ()
drawColumn [] = putStr "\n"
drawColumn ( (cellType, (_, _)) : t) = do
    putStr (show cellType)
    drawColumn t

-- printBoardAux :: Board -> Int -> Int -> [[Char]]
-- printBoardAux board index rows
--     | index == rows = [""]
--     | otherwise = ["", printRow (getRowByIndex board Int Int board index)] ++ printBoardAux board (index+1) rows

-- printRow :: [[CellType]] -> [Char]
-- printRow [] = ""
-- printRow (cellType : cellTypes) = "" ++ show cellType ++ printRow cellTypes



