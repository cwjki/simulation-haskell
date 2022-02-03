

module Types (
    Board, CellType(Empty, Corral, Kid, Obstacle, Dirt, Robot), Cell,
    State (Regular ,WithKid, OnDirt, OnCorral, OnCorrallWithKid),
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
import Data.List (sortOn)


data CellType = Empty | Kid | Obstacle | Corral | Dirt | Robot deriving (Eq)

data State = Regular | WithKid | OnDirt | OnCorral | OnCorrallWithKid deriving (Eq)

type Position = (Int, Int)

type Cell = (CellType, Position, State, Position)

type Board = [[Cell]]


-- Cell

getCell :: Board -> Int -> Int -> Cell
getCell board row column = board !! row !! column

getCellType :: Cell -> CellType
getCellType (cellType, _, _, _) = cellType

getPosition :: Cell -> Position
getPosition (_, position, _, _) = position

getCellRow :: Cell -> Int
getCellRow (_, position, _, _) = fst position

getCellColumn :: Cell -> Int
getCellColumn (_, position, _ , _) = snd position

getCellTargetRow :: Cell -> Int
getCellTargetRow (_, _, _, targetPos) = fst targetPos

getCellTargetCol :: Cell -> Int
getCellTargetCol (_, _, _, targetPos) = snd targetPos

getTargetCell :: Board -> Cell -> Cell
getTargetCell board cell = board !! getCellTargetRow cell !! getCellTargetCol cell

getCellState :: Cell -> State
getCellState (_, _, state, _) = state

getAdjacentCells :: Cell -> Board -> [Cell]
getAdjacentCells (cellType, (row, column), _, _) board = adjacentCells where
    rowLength = length board
    columnLength = length (head board)
    up = [board !! (row-1) !! column | row /= 0]
    left = [board !! row !! (column - 1) | column /= 0]
    down = [board !! (row+1) !! column | row /= rowLength-1]
    rigth = [board !! row !! (column+1) | column /= columnLength-1]

    adjacentCells = up ++ left ++ down ++ rigth

getAdjacentCellsList :: [Cell] -> Board -> [Cell]
getAdjacentCellsList [] _ = []
getAdjacentCellsList (h : t) board = getAdjacentCells h board ++ getAdjacentCellsList t board

getAllAdjacentCells :: Cell -> Board -> [Cell]
getAllAdjacentCells (cellType, (row, column), _, _) board = adjacentCells where
    rowLength = length board
    columnLength = length (head board)
    up        = [board !! (row-1) !! column  | row /= 0]
    left      = [board !! row !! (column-1)      | column /= 0]
    down      = [board !! (row+1) !! column      | row /= rowLength-1]
    rigth     = [board !! row !! (column+1)      | column /= columnLength-1]
    upLeft    = [board !! (row-1) !! (column-1)  | row /= 0 && column /= 0]
    upRigth   = [board !! (row-1) !! (column+1)  | row /= 0 && column /= columnLength-1]
    downLeft  = [board !! (row+1) !! (column-1)  | row /= rowLength-1 && column /= 0]
    downRigth = [board !! (row+1) !! (column+1) | row /= rowLength-1 && column /= columnLength-1]

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

getCellsWithDistances :: Cell -> [Cell] -> [(Cell, Int)]
getCellsWithDistances _ [] = []
getCellsWithDistances origin (cell : t) =
  let distance = getDistance origin cell
   in (cell, distance) : getCellsWithDistances origin t



replace :: [a] -> Int -> a -> [a]
replace list index element =
  let (first, x : xs) = splitAt index list in first ++ (element : xs)

-- replace a Cell on the Board
replaceCell :: Cell -> Board -> Board
replaceCell (cellType, (row, column), state, (targetRow, targetCol)) board =
  replace board row (replace (board !! row) column (cellType, (row, column), state, (targetRow, targetCol)))

-- given a list of Cells update the Board
replaceCellList :: [Cell] -> Board -> Board
replaceCellList [] board = board
replaceCellList ((cellType, (row, column), state, targetPos) : t) board = replaceCellList t (replace board row (replace (board !! row) column (cellType, (row, column), state, targetPos)))


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
    | getCellType cell == Kid = replaceCell (Empty, (getCellRow cell, getCellColumn cell), Regular, (-1, -1)) board
    | otherwise = newBoard where
        row = getCellRow cell
        col = getCellColumn cell
        newRow = row + rowDir
        newCol = col + colDir
        newCellType = getCellType (board !! newRow !! newCol)
        boardAux = replaceCell (newCellType, (row, col), Regular, (-1, -1)) board
        newBoard = _moveObstacles boardAux (board !! newRow !! newCol) rowDir colDir


-- return opposite direction 
getOppositeDir :: Int -> Int -> (Int, Int)
getOppositeDir rowDir colDir
    | rowDir == 0 = (0, -colDir)
    | colDir == 0 = (-rowDir, 0)
    | otherwise = (rowDir, colDir)



-- try to find the firt empty cell in a direction, can return a non empty cell 
getFirtsEmptyCell :: Board -> Cell -> Int -> Int -> Cell
getFirtsEmptyCell board (cellType, (row, column), state, targetPos) dirRow dirCol =
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



-- return a list with the Cell and the Distace to all reacheable cells 
bfsDistance :: Board -> Int -> [Cell] -> [Cell] -> [(Cell, Int)] -> [(Cell, Int)]
bfsDistance _ _ [] _ allDistances = allDistances
bfsDistance board count (cell : queue) visited allDistances
    | cell `elem` visited = bfsDistance board count queue visited allDistances
    | otherwise = newAllDistances where
        adjacentCells   = getAdjacentCells cell board
        possibleCells   = getPossibleWalkCells adjacentCells
        newQueue        = queue ++ possibleCells
        newVisited      = visited ++ [cell]
        tuples          = getDistanceTuple count possibleCells
        newAllDistances = bfsDistance board (count+1) newQueue newVisited (allDistances ++ tuples)

filterByReachable :: [Cell] -> [(Cell, Int)] -> [(Cell, Int)]
filterByReachable [] reachableCells = []
filterByReachable (cell: t) reachableCells = result where
    tuple = getCellTuple cell reachableCells
    result = tuple ++ filterByReachable t reachableCells


getCellTuple :: Cell -> [(Cell, Int)] -> [(Cell, Int)]
getCellTuple _ [] = []
getCellTuple cell (h : t)
    | cell == fst h = [h]
    | otherwise = getCellTuple cell t


getDistanceTuple :: Int -> [Cell] -> [(Cell, Int)]
getDistanceTuple _ [] = []
getDistanceTuple distance (h : t) = (h, distance) : getDistanceTuple distance t


bfsLoop :: Board -> Cell -> [Cell] -> [Cell] -> [(Cell, Cell)] -> [(Cell, Cell)]
bfsLoop _ _ [] _ _ = []
bfsLoop board destinyCell (cell : queue) visited path
    | destinyCell == cell = path
    | cell `elem` visited = bfsLoop board destinyCell queue visited path
    | otherwise = newPath where
        adjacentCells = getAdjacentCells cell board
        possibleCells = getPossibleWalkCells adjacentCells
        newQueue      = queue ++ possibleCells
        newVisited    = visited ++ [cell]
        tuple         = getPathTuple cell possibleCells
        newPath = bfsLoop board destinyCell newQueue newVisited (path ++ tuple)


-- getPath :: (Cell, Cell) -> (Cell, Cell) -> [(Cell, Cell)] -> [(Cell, Cell)] -> [(Cell, Cell)]
-- getPath origin destiny path newPath =
--     | origin == destiny 




getPathTuple :: Cell -> [Cell] -> [(Cell, Cell)]
getPathTuple _ [] = []
getPathTuple cell (h : t) = (cell,h) : getPathTuple cell t


-- get the possible cells where a robot can walk
getPossibleWalkCells :: [Cell] -> [Cell]
getPossibleWalkCells adjacentCells = possibleCells where
    emptyCells  = filterByCellTypeList Empty adjacentCells
    dirtCells   = filterByCellTypeList Dirt adjacentCells
    corralCells = filterByCellTypeList Corral adjacentCells
    possibleCells = emptyCells ++ dirtCells ++ corralCells


-- Robots Logic

getAllRobotsTargets :: Board -> [Cell] -> [Cell]
getAllRobotsTargets _ [] = []
getAllRobotsTargets board (robot: t) = allTargets where
    targetRow = getCellTargetRow robot
    targetCol = getCellTargetCol robot 
    allTargets
        | targetRow == -1 && targetCol == -1 = getAllRobotsTargets board t
        | otherwise = getTargetCell board robot : getAllRobotsTargets board t

getAllRobotsTarget2 :: Board -> [Cell]
getAllRobotsTarget2 board = 
    let robots = filterByCellType Robot board
    in map (getTargetCell board) robots


checkRobotsTasks :: Board -> [Cell] -> Board
checkRobotsTasks board [] = board
checkRobotsTasks board (robot : t) =
    let targetRow = getCellTargetRow robot
        targetCol = getCellTargetCol robot
        newBoard
         | targetRow == -1 && targetCol == -1 = getNewRobotTask board robot t
         | otherwise = board
     in checkRobotsTasks newBoard t


--assign a task to all robots 
computeRobotTasks :: Board -> [Cell] -> Board
computeRobotTasks board [] = board
computeRobotTasks board (robot : t) = computeRobotTasks (getNewRobotTask board robot t) t 

-- assign a new target to a robot
getNewRobotTask :: Board -> Cell -> [Cell] -> Board
getNewRobotTask board robot otherRobots = newBoard where
    allDistances = bfsDistance board 1 [robot] [] []
    kids = filterByCellType Kid board
    dirt = filterByCellType Dirt board
    nonTargetKids = getNonTargetCells board kids otherRobots
    nonTargetDirts = getNonTargetCells board dirt otherRobots
    reachableKids = filterByReachable nonTargetKids allDistances
    reachableDirts = filterByReachable nonTargetDirts allDistances
    newBoard
     | not (null reachableKids) =
        let sortedTarget = sortOn snd reachableKids
            target = fst (head sortedTarget)
            rowTarget = getCellRow target 
            colTarget = getCellColumn target 
            row       = getCellRow robot
            col       = getCellColumn robot
            state     = getCellState robot
         in replaceCell (Robot, (row, col), state, (rowTarget, colTarget)) board

     | not (null reachableDirts) =
        let sortedTarget = sortOn snd reachableDirts
            target = fst (head sortedTarget)
            rowTarget = getCellRow target 
            colTarget = getCellColumn target 
            row       = getCellRow robot
            col       = getCellColumn robot
            state     = getCellState robot
         in replaceCell (Robot, (row, col), state, (rowTarget, colTarget)) board
     | otherwise = board



getNonTargetCells :: Board -> [Cell] -> [Cell] -> [Cell]
getNonTargetCells board cells robots = _getNonTargetCells board cells robots 0

_getNonTargetCells :: Board -> [Cell] -> [Cell] -> Int -> [Cell]
_getNonTargetCells board cells robots index
    | index == length robots = []
    | otherwise = nonTargetCells where
        cell = cells !! index
        isTarget = cell `elem` getAllRobotsTargets board robots
        nonTargetCells
            | isTarget = _getNonTargetCells board cells robots (index+1)
            | otherwise = cell : _getNonTargetCells board cells robots (index+1)


-- movesRobots :: Board -> Board
-- movesRobots board = newBoard where
--     robots = filterByCellType (Robot _ _) board
--     robotsWithTask = computeRobotTasks board robots
--     newBoard = _moveRobots robotsWithTask (filterByCellType (Robot State Task) board)

-- getAllRobots :: Board -> [Cell]
-- getAllRobots board =
--     let rowLength = length board
--         colLength = length (head board)
--      in _getAllRobots board rowLength colLength 0 0


-- _getAllRobotsRows :: Board -> Int -> Int -> Int -> Int -> [Cell]
-- _getAllRobotsRows board rowLength colLength row
--     | row == rowLength = []
--     | otherwise = _getAllRobotsCols board colLength row 0 ++ _getAllRobotsRows board rowLength colLength (row+1)

-- _getAllRobotsCols :: Board -> Int -> Int -> Int -> [Cell]
-- _getAllRobotsCols board colLength row col
--   | col == colLength = []
--   | otherwise =
--       let cell = board !! row !! col
--           cellType = getCellType cell
--           possibleRobot
--             | cellType == Robot  = [cell]
--             | otherwise = []
--        in possibleRobot ++ _getAllRobotsCols


-- _moveRobots :: Board -> [Cell] -> Board
-- _moveRobots board _ = board
-- -- _moverobots board (robot : t) =  

