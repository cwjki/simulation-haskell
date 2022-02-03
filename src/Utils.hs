
module Utils
    ( printBoard
    , randomGen
    ) where

import System.Random ( Random(randomR), StdGen )
import Types ( Board, CellType (Dirt, Empty, Obstacle, Corral, Kid, Robot), Cell, State (Regular, WithKid, OnCorral, OnDirt, OnCorrallWithKid) )


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

drawBoard :: [[(CellType, (Int, Int), State, (Int, Int))]] -> Int -> IO ()
drawBoard []      _     = putStr "\n"
drawBoard (h : t) count = do
    putStr (show count)
    drawRow h
    drawBoard t (count + 1)

drawRow :: [(CellType, (Int, Int), State, (Int, Int))] -> IO ()
drawRow [] = putStr "\n"
drawRow ((Empty , (_, _), _, _) : t) = do
    putStr "[   ]"
    drawRow t
drawRow ((Dirt, (_, _), _, _) : t) = do
  putStr "[ D ]"
  drawRow t
drawRow ((Obstacle , (_, _), _, _) : t) = do
  putStr "[ O ]"
  drawRow t
drawRow ((Corral, (_, _), _, _) : t) = do
  putStr "[ C ]"
  drawRow t
drawRow ((Kid, (_, _), OnCorral, _) : t) = do
  putStr "[KC ]"
  drawRow t
drawRow ((Kid, (_, _), _, _) : t) = do
  putStr "[ K ]"
  drawRow t
drawRow ((Robot, (_, _), Regular, _) : t) = do
  putStr "[ R ]"
  drawRow t
drawRow ((Robot, (_, _), WithKid , _) : t) = do
  putStr "[RK ]"
  drawRow t
drawRow ((Robot, (_, _), OnDirt , _) : t) = do
  putStr "[RD ]"
  drawRow t
drawRow ((Robot, (_, _), OnCorral, _) : t) = do
  putStr "[RC ]"
  drawRow t
drawRow ((Robot, (_, _), OnCorrallWithKid, _) : t) = do
  putStr "[RKC ]"
  drawRow t
