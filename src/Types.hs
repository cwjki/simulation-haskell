
module Types (
    Board, CellType(Empty, Corral, Kid, Obstacle, Dirt, Robot), Cell,
    State (Regular ,WithKid, OnDirt, OnCorral, OnCorrallWithKid),
    TaskType (GrabKid, Clean),
    filterByCellType,
    filterByCellTypeList,
    getEmptyAdjacentCells,
    getCellRow,
    getCellColumn,
    getFirtsEmptyCell,
    moveObstacles,
    replaceCell,
    replaceCellList,
    getCellType,
    getAdjacentCells,
    getAllAdjacentCells,
    get9Cells
    )
    where

import Debug.Trace
import System.Random (StdGen)


data CellType = Empty | Kid State | Obstacle | Corral | Dirt | Robot State Task deriving (Eq)

data State = Regular | WithKid | OnDirt | OnCorral | OnCorrallWithKid deriving (Eq)

data TaskType = NoTask | GrabKid | Clean deriving Eq

type Position = (Int, Int)

type Cell = (CellType, Position)

type Task = (TaskType, Cell)


type Board = [[Cell]]


instance Show CellType where
    show Empty                      = "[   ]"
    show Obstacle                   = "[ O ]"
    show Corral                     = "[ C ]"
    show Dirt                       = "[ D ]"
    show (Kid Regular)              = "[ K ]"
    show (Kid OnCorral )            = "[KC ]"
    show (Robot Regular _ )         = "[ R ]"
    show (Robot WithKid _)          = "[RK ]"
    show (Robot OnDirt _)           = "[RD ]"
    show (Robot OnCorral _)         = "[RC ]"
    show (Robot OnCorrallWithKid _) = "[RKC]"
    show (Kid _)                    = "[ E ]"



-- Cell

getCell :: Board -> Int -> Int -> Cell
getCell board row column = board !! row !! column

getCellType :: Cell -> CellType
getCellType (cellType, position) = cellType

getPosition :: Cell -> Position
getPosition (cellType, position) = position

getCellRow :: Cell -> Int
getCellRow (celltype, position) = fst position

getCellColumn :: Cell -> Int
getCellColumn (celltype, position) = snd position

getAdjacentCells :: Cell -> Board -> [Cell]
getAdjacentCells (cellType, (row, column)) board = adjacentCells where
    rowLength = length board
    columnLength = length (head board)
    up = [board !! (row-1) !! column | row /= 0]
    left = [board !! row !! (column - 1) | column /= 0]
    down = [board !! (row+1) !! column | row /= (rowLength-1)]
    rigth = [board !! row !! (column+1) | column /= (columnLength-1)]

    adjacentCells = up ++ left ++ down ++ rigth

getAdjacentCellsList :: [Cell] -> Board -> [Cell]
getAdjacentCellsList [] _ = []
getAdjacentCellsList (h : t) board = getAdjacentCells h board ++ getAdjacentCellsList t board

getAllAdjacentCells :: Cell -> Board -> [Cell]
getAllAdjacentCells (cellType, (row, column)) board = adjacentCells where
    rowLength = length board
    columnLength = length (head board)
    up        = [board !! (row-1) !! column  | row /= 0]
    left      = [board !! row !! (column-1)      | column /= 0]
    down      = [board !! (row+1) !! column      | row /= (rowLength-1)]
    rigth     = [board !! row !! (column+1)      | column /= (columnLength-1)]
    upLeft    = [board !! (row-1) !! (column-1)  | row /= 0 && column /= 0]
    upRigth   = [board !! (row-1) !! (column+1)  | row /= 0 && column /= (columnLength-1)]
    downLeft  = [board !! (row+1) !! (column-1)  | row /= (rowLength-1) && column /= 0]
    downRigth = [board !! (row+1) !! (column+1) | row /= (rowLength-1) && column /= (columnLength-1)]

    adjacentCells = up ++ left ++ down ++ rigth ++ upLeft ++ upRigth ++ downLeft ++ downRigth

get9Cells :: Cell -> Board -> [Cell]
get9Cells cell board = cell : getAllAdjacentCells cell board

getEmptyAdjacentCells :: [Cell] -> Board -> [Cell]
getEmptyAdjacentCells cells board = result where
    adjacentCells = getAdjacentCellsList cells board
    result = filterByCellTypeList Empty adjacentCells

getDistance :: Cell -> Cell -> Int
getDistance origin destiny = distance where
    originRow = getCellRow origin
    originCol = getCellColumn origin
    destinyRow = getCellRow destiny
    destinyCol = getCellColumn destiny
    rowDistance = (originRow - destinyRow) ^ 2
    colDistance = (originCol - destinyCol) ^ 2
    distance = rowDistance + colDistance




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


-- Board

-- return a list of cells with the desired cellType
filterByCellType :: CellType -> Board -> [Cell]
filterByCellType cellType board = filterByCellTypeRow cellType board 0 (length board)

filterByCellTypeRow :: CellType -> Board -> Int -> Int -> [Cell]
filterByCellTypeRow cellType board row rowLength
    | row == rowLength = []
    | otherwise = filterByCellTypeColumn cellType board row 0 (length (head board)) ++ filterByCellTypeRow cellType board (row+1) rowLength

filterByCellTypeColumn :: CellType -> Board -> Int -> Int -> Int -> [Cell]
filterByCellTypeColumn cellType board row column columnLength
    | column == columnLength = []
    | getCellType (getCell board row column) == cellType = getCell board row column : filterByCellTypeColumn cellType board row (column+1) columnLength
    | otherwise = filterByCellTypeColumn cellType board row (column+1) columnLength

filterByCellTypeList :: CellType -> [Cell] -> [Cell]
filterByCellTypeList _ [] = []
filterByCellTypeList cellType (h : t)
    | getCellType h == cellType = h : filterByCellTypeList cellType t
    | otherwise = filterByCellTypeList cellType t



-- return the i-th row 
getRowByIndex :: Board -> Int -> Int -> [Cell]
getRowByIndex board columns index = getRowByIndexAux board columns index 0

getRowByIndexAux :: Board -> Int -> Int -> Int -> [Cell]
getRowByIndexAux board columns index i
    | i == columns = []
    | otherwise = (board !! index !! i)
    : getRowByIndexAux board columns index (i + 1)


-- Kid 

moveObstacles :: Board -> Cell -> Int -> Int -> Board
moveObstacles board cell rowDir colDir =
    let (row, column) = getOppositeDir rowDir colDir
     in _moveObstacles board cell row column

_moveObstacles :: Board -> Cell -> Int -> Int -> Board
_moveObstacles board cell rowDir colDir
    | getCellType cell == Kid Regular = replaceCell (Empty, (getCellRow cell, getCellColumn cell)) board
    | otherwise = newBoard where
        row = getCellRow cell
        col = getCellColumn cell
        newRow = row + rowDir
        newCol = col + colDir
        newCellType = getCellType (board !! newRow !! newCol)
        boardAux = replaceCell (newCellType, (row, col)) board
        newBoard = _moveObstacles boardAux (board !! newRow !! newCol) rowDir colDir


-- return opposite direction 
getOppositeDir :: Int -> Int -> (Int, Int)
getOppositeDir rowDir colDir
    | rowDir == 0 = (0, -colDir)
    | colDir == 0 = (-rowDir, 0)
    | otherwise = (rowDir, colDir)



-- try to find the firt empty cell in a direction, can return a non empty cell 
getFirtsEmptyCell :: Board -> Cell -> Int -> Int -> Cell
getFirtsEmptyCell board (cellType, (row, column)) dirRow dirCol =
    let rowLength = length board
        columnLength = length (head board)
        destinyRow = row + dirRow
        destinyColumn  = column + dirCol
        emptyCell
            | destinyRow < 0 || destinyRow >= rowLength || destinyColumn < 0 || destinyColumn >= columnLength = board !! row !! column
            | getCellType(board !! destinyRow !! destinyColumn) == Empty = board !! destinyRow !! destinyColumn
            | getCellType(board !! destinyRow !! destinyColumn) == Obstacle = getFirtsEmptyCell board (board !! destinyRow !! destinyColumn) dirRow dirCol
            | otherwise = board !! row !! column
    in emptyCell



-- Robots Logic

getRobotTaskType :: Cell -> TaskType
getRobotTaskType ( Robot state (taskType, cell), (row, col) ) = taskType
getRobotTaskType cell = NoTask

getRobotTarget :: Cell -> Cell
getRobotTarget ( Robot state (taskType, cell), (row, col) ) = cell
getRobotTarget cell = cell

getAllRobotsTargets :: [Cell] -> [Cell]
getAllRobotsTargets = map getRobotTarget

-- movesRobots :: Board -> Board
-- movesRobots board = 
--     let robots = filterByCellType (Robot _) board
--         robotsWithTask = 



-- checkRobotsTasks :: Board -> [Cell] -> [Cell]
-- checkRobotsTasks board [] = filterByCellType (Robot _) board
-- checkRobotsTasks board (robot : t) = 
--     let actualRobotTaskType = getRobotTaskType robot
--         result 
--             | actualRobotTaskType == NoTask = 
--                 let robotTask = getNewRobotTask board robot t


-- getNewRobotTask :: Board -> Cell -> [Cell] -> Task
-- getNewRobotTask board robot otherRobots = newTask where




getNonTargetCells :: [Cell] -> [Cell] -> [Cell]
getNonTargetCells cells robots = _getNonTargetCells cells robots 0

_getNonTargetCells :: [Cell] -> [Cell] -> Int -> [Cell]
_getNonTargetCells cells robots index
    | index == length robots = []
    | otherwise = nonTargetCells where
        cell = cells !! index
        isTarget = cell `elem` getAllRobotsTargets robots
        nonTargetCells
            | isTarget = _getNonTargetCells cells robots (index+1)
            | otherwise = cell : _getNonTargetCells cells robots (index+1)


-- _moveRobots :: Board -> [Cell] -> Board
-- _moveRobots board robots = board



