module Simulations
    ( simulate
    ) where

import Debug.Trace
import           System.Random                  ( StdGen
                                                , mkStdGen
                                                )
import           Types                          ( Board
                                                , Cell
                                                , CellType(Empty, Corral, Kid, Obstacle, Robot, Dirt)
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
generateBoard n m robots kids obstacles dirts seed = board where  
    emptyBoard = generateEmptyBoard n m
    corralBoard = generateCorrals emptyBoard kids (mkStdGen seed)
    kidBoard = generateStuff corralBoard Kid kids (mkStdGen (seed+5))
    obstacleBoard = generateStuff kidBoard Obstacle obstacles (mkStdGen (seed + 103))
    dirtBoard = generateStuff obstacleBoard Dirt dirts (mkStdGen (seed + 4))
    board = generateStuff dirtBoard Robot robots (mkStdGen (seed + 43))



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


-- generate Kids
generateKids :: Board -> Int -> StdGen -> Board
generateKids board kidsCount seed 
    | kidsCount == 0 = board 
    | otherwise = 
        let emptyCells = filterByCellType Empty board
            (rIndex, newSeed) = randomGen 0 (length emptyCells -1) seed
            (_ ,(row, column)) = emptyCells !! rIndex
            newBoard = replaceCell (Kid, (row, column)) board
        in generateKids newBoard (kidsCount-1) newSeed


-- generate Obstacles
generateObstacles :: Board -> Int -> StdGen -> Board
generateObstacles board obstacleCount seed
    | obstacleCount == 0 = board
    | otherwise = 
        let emptyCells              = filterByCellType Empty board
            (rIndex, newSeed      ) = randomGen 0 (length emptyCells - 1) seed
            (_     , (row, column)) = emptyCells !! rIndex
            newBoard                = replaceCell (Obstacle , (row, column)) board
        in generateObstacles newBoard (obstacleCount - 1) newSeed


-- generate Robots
generateRobots :: Board -> Int -> StdGen -> Board
generateRobots board robotCount seed
    | robotCount == 0 = board
    | otherwise = 
        let emptyCells              = filterByCellType Empty board
            (rIndex, newSeed      ) = randomGen 0 (length emptyCells - 1) seed
            (_     , (row, column)) = emptyCells !! rIndex
            newBoard                = replaceCell (Robot, (row, column)) board
        in generateRobots newBoard (robotCount - 1) newSeed


generateStuff :: Board -> CellType  ->  Int -> StdGen  -> Board 
generateStuff board cellType count seed
    | count == 0 = board
    | otherwise = 
        let emptyCells              = filterByCellType Empty board
            (rIndex, newSeed      ) = randomGen 0 (length emptyCells - 1) seed
            (_     , (row, column)) = emptyCells !! rIndex
            newBoard                = replaceCell (cellType, (row, column)) board
        in generateStuff newBoard cellType (count - 1) newSeed
