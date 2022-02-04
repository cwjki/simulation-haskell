module Simulations
    ( startSimulation
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



startSimulation ::  Int ->Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
startSimulation n m robots kids obstacles dirt seed t k = do
    let board = generateBoard n m robots kids obstacles dirt seed
     in mainLoop board 0 t k seed


mainLoop :: Board -> Int -> Int -> Int -> Int -> IO ()
mainLoop board turn t k seed 
    | turn == k = putStrLn "Fin"
    |otherwise = do
        putStrLn $ "Turno " ++ show turn
        printBoard board
        let kidsMoves = moveKids board (mkStdGen (seed+turn))
            newBoard = movesRobots kidsMoves
         in mainLoop newBoard (turn+1) t k seed



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






