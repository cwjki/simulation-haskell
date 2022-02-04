module Simulations
    ( simulate
    ) where

import           Debug.Trace
import           System.Random                  ( StdGen
                                                , mkStdGen
                                                )
import           Types                          ( Board
                                                , Cell
                                                , CellType
                                                    ( Corral
                                                    , Dirt
                                                    , Empty
                                                    , Kid
                                                    , Obstacle
                                                    , Robot
                                                    )
                                                , State (Regular ,WithKid, OnDirt, OnCorral, OnCorralWithKid)
                                                , filterByCellType
                                                , getEmptyAdjacentCells
                                                , moveObstacles
                                                , replaceCell
                                                , replaceCellList
                                                , getAdjacentCells
                                                , getCellRow
                                                , getCellColumn
                                                , getCellType
                                                , getFirtsEmptyCell, getAllAdjacentCells, filterByCellTypeList, get9Cells
                                                , movesRobots
                                                )
import           Utils                          ( printBoard
                                                , randomGen

                                                )


-- TO DEBUG
-- trace ("DEBUG: bobreverse" ++ show x)



simulate :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulate n m robots kids obstacles dirt seed = do
    let board = generateBoard n m robots kids obstacles dirt seed
        mKids1 = moveKids board (mkStdGen (seed + 4))
        mKids2 = moveKids mKids1 (mkStdGen (seed + 5))
        mKids3 = moveKids mKids2 (mkStdGen (seed + 6))
        mKids4 = moveKids mKids3 (mkStdGen (seed + 7))
        mKids5 = moveKids mKids4 (mkStdGen (seed + 8))
        mKids6 = moveKids mKids5 (mkStdGen (seed + 9))
        mKids7 = moveKids mKids6 (mkStdGen (seed + 99))
        mKids8 = moveKids mKids7 (mkStdGen (seed + 56))

        mRobot1 = movesRobots mKids8
        mRobot2 = movesRobots mRobot1
        mRobot3 = movesRobots mRobot2
        mRobot4 = movesRobots mRobot3
        mRobot5 = movesRobots mRobot4
        mRobot6 = movesRobots mRobot5        


    printBoard board
    
    print "Movimiento 1"
    printBoard mKids1
    print "Movimiento 2"
    printBoard mKids2
    print "Movimiento 3"
    printBoard mKids3
    print "Movimiento 4"
    printBoard mKids4
    print "Movimiento 5"
    printBoard mKids5
    print "Movimiento 6"
    printBoard mKids6
    print "Movimiento 7"
    printBoard mKids7
    print "Movimiento 8"
    printBoard mKids8

    print "Movimiento 9 Robot"
    printBoard mRobot1
    print "Movimiento 10"
    printBoard mRobot2
    print "Movimiento 11"
    printBoard mRobot3
    print "Movimiento 12"
    printBoard mRobot4
    print "Movimiento 13"
    printBoard mRobot5
    print "Movimiento 14"
    printBoard mRobot6





generateBoard :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Board
generateBoard n m robots kids obstacles dirts seed = board  where
    emptyBoard    = generateEmptyBoard n m
    corralBoard   = generateCorrals emptyBoard kids (mkStdGen seed)
    kidBoard      = generateStuff corralBoard Kid kids (mkStdGen (seed + 5))
    obstacleBoard = generateStuff kidBoard Obstacle obstacles (mkStdGen (seed + 103))
    dirtBoard     = generateStuff obstacleBoard Dirt dirts (mkStdGen (seed + 4))
    board         = generateStuff dirtBoard Robot robots (mkStdGen (seed + 43))



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
    | otherwise = (Empty, (row, column), Regular, (-1,-1))
    : generateEmptyColumns columns row (column + 1)



-- generate Corrals
generateCorrals :: Board -> Int -> StdGen -> Board
generateCorrals board corralCount seed
    | corralCount == 0
    = board
    | null (filterByCellType Corral board)
    = let emptyCells              = filterByCellType Empty board
          (rIndex, newSeed      ) = randomGen 0 (length emptyCells - 1) seed
          (_     , (row, column), _, _) = emptyCells !! rIndex
          newBoard                = replaceCell (Corral, (row, column), Regular, (-1,-1)) board
      in  generateCorrals newBoard (corralCount - 1) newSeed
    | otherwise
    = let corralCells             = filterByCellType Corral board
          emptyAdjacentCells      = getEmptyAdjacentCells corralCells board
          (rIndex, newSeed) = randomGen 0 (length emptyAdjacentCells - 1) seed
          (_     , (row, column), _, _) = emptyAdjacentCells !! rIndex
          newBoard                = replaceCell (Corral, (row, column), Regular, (-1, -1)) board
      in  generateCorrals newBoard (corralCount - 1) newSeed



-- generate cellTypes objects <robots, kids, dirt, obstacles> in random cells 
generateStuff :: Board -> CellType -> Int -> StdGen -> Board
generateStuff board cellType count seed
    | count == 0
    = board
    | otherwise
    = let emptyCells              = filterByCellType Empty board
          (rIndex, newSeed      ) = randomGen 0 (length emptyCells - 1) seed
          (_     , (row, column), _, _) = emptyCells !! rIndex
          newBoard                = replaceCell (cellType, (row, column), Regular, (-1,-1)) board
      in  generateStuff newBoard cellType (count - 1) newSeed


--- KIDS 
moveKids :: Board -> StdGen -> Board
moveKids board seed = newBoard where
    kids =  filterByCellType Kid board
    newBoard = _moveKids board kids seed


_moveKids :: Board -> [Cell] -> StdGen -> Board
_moveKids board [] _ = board
_moveKids board (kidCell : t) seed =
    let (kidStay, newSeed) = randomGen 0 1 seed
        newBoard -- check if the kid wnats to move in this turn 
            | kidStay == 1 = board
            | otherwise =
                let possibleCells = getAdjacentCells kidCell board
                    (rIndex, newSeed) = randomGen 0 (length possibleCells -1) seed
                    choosenCell = possibleCells !! rIndex
                    boardAux1 -- check if the kid want to move to an empty cell or can push some obstacles
                        | getCellType choosenCell == Empty = 
                            let boardWithKidMove = replaceCell (Empty, (getCellRow kidCell, getCellColumn kidCell), Regular, (-1,-1)) (replaceCell (Kid, (getCellRow choosenCell, getCellColumn choosenCell), Regular, (-1,-1)) board)
                             in kidGenerateDirt boardWithKidMove (boardWithKidMove !! getCellRow kidCell !! getCellColumn kidCell) newSeed
                        
                        | getCellType choosenCell == Obstacle =
                            let rowDir = getCellRow choosenCell - getCellRow kidCell
                                colDir = getCellColumn choosenCell - getCellColumn kidCell
                                destinyCell = getFirtsEmptyCell board choosenCell rowDir colDir
                                boardAux2 -- check if the kid can push the obstacles to move
                                    | getCellType destinyCell == Empty = 
                                        let boardWithKidMove = moveObstacles board destinyCell rowDir colDir
                                         in kidGenerateDirt boardWithKidMove (boardWithKidMove !! getCellRow kidCell !! getCellColumn kidCell) newSeed
                                    | otherwise = board                       
                             in boardAux2

                        | otherwise = board
                 in boardAux1
     in _moveKids newBoard t newSeed


-- Generate dirt after a kid moves
kidGenerateDirt :: Board -> Cell -> StdGen -> Board 
kidGenerateDirt board kidCell seed = newBoard where
    cells = get9Cells kidCell board
    kidsCount = length (filterByCellTypeList Kid cells)
    emptyCells = filterByCellTypeList Empty cells
    emptyCount = length emptyCells
    newBoard = putDirt board emptyCells kidsCount emptyCount seed

putDirt :: Board -> [Cell] -> Int -> Int -> StdGen  -> Board 
putDirt board cells 1 emptyCount seed         = generateDirt board cells 1 seed
putDirt board cells 2 emptyCount seed         = generateDirt board cells 3 seed
putDirt board cells kidsCount emptyCount seed = generateDirt board cells 6 seed


-- put or not n dirt cell in a grid 3x3 
generateDirt :: Board -> [Cell] -> Int -> StdGen -> Board
generateDirt board [] _ _ = board
generateDirt board _ 0 _ = board
generateDirt board emptyCells dirtCount seed = 
    let (genDirt, newSeed) = randomGen 0 1 seed 
        newBoard 
            | genDirt == 0 = generateDirt board emptyCells (dirtCount-1) newSeed
            | otherwise = 
                let (rIndex, newSeed1) = randomGen 0 (length emptyCells -1) newSeed
                    (cellType, (row, col), _, _) = emptyCells !! rIndex
                    dirtyBoard = replaceCell (Dirt, (row, col), Regular, (-1,-1)) board
                 in generateDirt dirtyBoard (filterByCellTypeList Empty emptyCells) (dirtCount-1) newSeed1
     in newBoard


-- ROBOTS






