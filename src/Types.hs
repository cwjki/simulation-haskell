
module Types (
    Board, CellType(Empty, Corral, Kid, Obstacle, Dirt, Robot), Cell,
    filterByCellType,
    getEmptyAdjacentCells,
    getCellRow,
    getCellColumn,
    getFirtsEmptyCell,
    moveObstacles,
    replaceCell,
    replaceCellList,
    getCellType,
    getAdjacentCells
    )
    where

import Debug.Trace
import System.Random (StdGen)


data CellType = Empty | Kid | Obstacle | Corral | Dirt
                | Robot | RobotKid | RobotDirt | RobotCorral
                | KidCorral | RobotKidCorral
                deriving (Eq)

type Position = (Int, Int)

type Cell = (CellType, Position)

type Board = [[Cell]]


instance Show CellType where
    show Empty          = "[   ]"
    show Kid            = "[ K ]"
    show Obstacle       = "[ O ]"
    show Corral         = "[ C ]"
    show Dirt           = "[ D ]"
    show Robot          = "[ R ]"
    show RobotKid       = "[RK ]"
    show RobotDirt      = "[RD ]"
    show RobotCorral    = "[RC ]"
    show KidCorral      = "[KC ]"
    show RobotKidCorral = "[RKC]"



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


getEmptyAdjacentCells :: [Cell] -> Board -> [Cell]
getEmptyAdjacentCells cells board = result where
    adjacentCells = getAdjacentCellsList cells board
    result = filterByCellTypeList Empty adjacentCells

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
    | getCellType cell == Kid = board
    | otherwise = newBoard where 
        row = getCellRow cell
        col = getCellColumn cell
        newRow = row + rowDir
        newCol = col + colDir
        newCellType = getCellType cell
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










