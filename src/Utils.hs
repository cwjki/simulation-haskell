module Utils where 

import System.Random


randomGen :: Int -> Int -> StdGen -> (Int, StdGen)
randomGen min max = randomR (min, max)


-----VISUAL-----

printBoard :: [[Char]] -> IO ()
printBoard [] = return ()
printBoard (row : rows) = do
    print row
    printBoard rows


printBoardAux :: Board -> Int -> Int -> [[Char]]
prinBoardAux board index rows
    | index == rows = [""]
    | otherwise = ["", printRows (getRow board index)] ++ printBoardAux board (index+1)

printRow :: [[CellType]] -> [Char]
printRow [] = ""
printRow (cellType : cellTypes)
    | null cellType = "" ++ printRow cellTypes
    | otherwise = "" ++ [celltype, " "] ++ printRow cellTypes




