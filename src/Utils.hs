module Utils where 

import System.Random
import Types


randomGen :: Int -> Int -> StdGen -> (Int, StdGen)
randomGen min max = randomR (min, max)


-----VISUAL-----

printBoard :: [[Char]] -> IO ()
printBoard [] = return ()
printBoard (row : rows) = do
    print row
    printBoard rows


printBoardAux :: Board -> Int -> Int -> [[Char]]
printBoardAux board index rows
    | index == rows = [""]
    | otherwise = ["", printRow (getRow board index)] ++ printBoardAux board (index+1) rows

printRow :: [[CellType]] -> [Char]
printRow [] = ""
printRow (cellType : cellTypes) = "" ++ show cellType ++ printRow cellTypes




drawRow :: Board -> Int -> Int -> 