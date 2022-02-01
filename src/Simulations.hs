module Simulations
    ( simulate
    ) where

import Debug.Trace
import           System.Random                  ( StdGen
                                                , mkStdGen
                                                )
import           Types                          ( Board
                                                , Cell
                                                , CellType(Empty, Corral)
                                                , filterByCellType
                                                , getEmptyAdjacentCells
                                                )
import           Utils                          ( printBoard
                                                , randomGen
                                                , replaceCell
                                                , replaceCellList
                                                )



simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m robots kids obstacles dirt seed = do
    let board = generateBoard n m robots kids obstacles dirt seed
    printBoard board






generateBoard :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Board
generateBoard n m robots kids obstacles dirt seed = board where 
    randomSeed = mkStdGen seed
    emptyBoard = generateEmptyBoard n m
    board = generateCorrals emptyBoard kids randomSeed



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
generateCorrals :: Board -> Int -> StdGen -> Board
generateCorrals board corralCount seed
    | corralCount == 0 = board
    | null (filterByCellType Corral board) = 
        let emptyCells        = filterByCellType Empty board
            (rIndex, newSeed) = randomGen 0 (length emptyCells -1) seed
            (_ ,(row, column))       = emptyCells !! rIndex
            newBoard          = replaceCell (Corral, (row, column)) board
        in  generateCorrals newBoard (corralCount-1) newSeed
    | otherwise =
        let corralCells = filterByCellType Corral board
            emptyAdjacentCells = getEmptyAdjacentCells corralCells board
            (rIndex, newSeed) = randomGen 0 (length emptyAdjacentCells - 1) seed
            (_, (row, column)) = emptyAdjacentCells !! rIndex
            newBoard = replaceCell (Corral, (row, column)) board
        in generateCorrals newBoard (corralCount-1) newSeed









-- generate Robots

-- generate Obstacles


-- generate Kids 


-- generate Dirt

