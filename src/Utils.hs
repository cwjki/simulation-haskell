module Utils
    ( printBoard
    , randomGen
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
    let index = [0 .. n-1]
    drawIndexAux index

drawIndexAux :: [Int] -> IO ()
drawIndexAux []      = putStr "\n"
drawIndexAux (h : t) = do
    putStr "   "
    putStr (show h)
    putStr " "
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



