module Types (
    Board, CellType(Empty, Corral, Kid, Obstacle, Dirt, Robot), Cell,
    State (Regular ,WithKid, OnDirt, OnCorral, OnCorralWithKid, OnDirtWithKid),
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
    get9Cells,
    movesRobots
    )
    where

import Debug.Trace
import System.Random (StdGen)
import Data.List (sortOn)


data CellType = Empty | Kid | Obstacle | Corral | Dirt | Robot deriving (Eq)

data State = Regular | WithKid | OnDirt | OnCorral | OnCorralWithKid | OnDirtWithKid deriving (Eq)

type Position = (Int, Int)

type Cell = (CellType, Position, State, Position)

type Board = [[Cell]]

--trace ("DEBUG: bobreverse" ++ show x)
instance Show CellType where
    show Empty = "[   ]"
    show Kid =   "[ K ]"
    show Obstacle = "[  O ]"
    show Corral = "[ C ]"
    show Dirt = "[ D ]"
    show Robot =   "[ R ]"

instance Show State where
  show Regular = "regular"
  show WithKid = "withkid"
  show OnDirt = "ondirt"
  show OnCorral = "oncorral"
  show OnCorralWithKid = "oncorralwithkid"
  show OnDirtWithKid = "ondirtWithKid"


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
bfsDistance :: Board -> [(Cell, Int)] -> [Cell] -> [(Cell, Int)] -> [(Cell, Int)]
bfsDistance _ [] _ allDistances = allDistances
bfsDistance board (h : queue) visited allDistances
    | fst h `elem` visited = bfsDistance board queue visited allDistances
    | otherwise = newAllDistances where
        cell  = fst h
        count = snd h
        adjacentCells = getAdjacentCells cell board
        possibleCells = getPossibleWalkCells adjacentCells
        tuples = getDistanceTuple (count+1) possibleCells
        newQueue = queue ++ tuples
        newVisited = visited ++ [cell]
        newAllDistances = bfsDistance board newQueue newVisited (allDistances ++ [h])

bfsDistanceKidsPatch :: Board -> [Cell] -> [(Cell, Int)] -> [(Cell, Int)]
bfsDistanceKidsPatch _ [] newAllDistance = newAllDistance
bfsDistanceKidsPatch board (kid : t) allDistance = result where
    adjacentCells = getAdjacentCells kid board
    reachableCells = filterByReachable adjacentCells allDistance
    closestCell
        | not (null reachableCells) =  [head (sortOn snd reachableCells)]
        | otherwise = []
    result
        | not (null closestCell) = (kid, snd (head closestCell) + 1) : bfsDistanceKidsPatch board t allDistance
        | otherwise = bfsDistanceKidsPatch board t allDistance

filterByReachable :: [Cell] -> [(Cell, Int)] -> [(Cell, Int)]
filterByReachable [] reachableCells = []
filterByReachable (cell : t) reachableCells = result where
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

-- bfs to find the path to the targetCell
bfsLoop :: Board -> Cell -> [Cell] -> [Cell] -> [(Cell, Cell)] -> [(Cell, Cell)]
bfsLoop _ _ [] _ _ = []
bfsLoop board destinyCell (cell : queue) visited path
    | destinyCell == cell = path
    | cell `elem` visited = bfsLoop board destinyCell queue visited path
    | otherwise = newPath where
        adjacentCells = getAdjacentCells cell board
        possibleCells = getPossibleWalkCells1 adjacentCells destinyCell
        newQueue      = queue ++ possibleCells
        newVisited    = visited ++ [cell]
        tuple         = getPathTuple cell possibleCells
        newPath = bfsLoop board destinyCell newQueue newVisited (path ++ tuple)


getPossibleWalkCells1 :: [Cell] -> Cell -> [Cell]
getPossibleWalkCells1 adjacentCells destinyCell = possibleCells where
    emptyCells = filterByCellTypeList Empty adjacentCells
    dirtCells = filterByCellTypeList Dirt adjacentCells
    corralCells = filterByCellTypeList Corral adjacentCells
    emptyCorrals = getEmptyCorrals corralCells
    possibleCells
        | destinyCell `elem` adjacentCells = emptyCells ++ dirtCells ++ emptyCorrals ++ [destinyCell]
        | otherwise = emptyCells ++ dirtCells ++ emptyCorrals

-- return the path to the targetCell
getPath :: Cell -> Cell -> [(Cell, Cell)] -> [Cell] -> [Cell]
getPath origin destiny path newPath
    | origin == destiny = newPath
    | otherwise = finalPath where
        tuple = getTupleByChild destiny path
        finalPath
            | null tuple = []
            | otherwise =  getPath origin (fst (head tuple)) path (fst (head tuple) : newPath)



-- aux method to getPath
getTupleByChild :: Cell -> [(Cell, Cell)] -> [(Cell, Cell)]
getTupleByChild _ [] = []
getTupleByChild cell (h : t)
    | cell == snd h = [h]
    | otherwise = getTupleByChild cell t

-- aux method to bfs loop
getPathTuple :: Cell -> [Cell] -> [(Cell, Cell)]
getPathTuple _ [] = []
getPathTuple cell (h : t) = (cell,h) : getPathTuple cell t


-- get the possible cells where a robot can walk
getPossibleWalkCells :: [Cell] -> [Cell]
getPossibleWalkCells adjacentCells = possibleCells where
    emptyCells  = filterByCellTypeList Empty adjacentCells
    dirtCells   = filterByCellTypeList Dirt adjacentCells
    corralCells = filterByCellTypeList Corral adjacentCells
    emptyCorrals = getEmptyCorrals corralCells
    possibleCells = emptyCells ++ dirtCells ++ emptyCorrals




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
         | targetRow == -1 && targetCol == -1 = getNewRobotTask board robot
         | otherwise = board
     in checkRobotsTasks newBoard t


--assign a task to all robots 
computeRobotTasks :: Board -> [Cell] -> Board
computeRobotTasks = foldl getNewRobotTask

-- assign a new target to a robot
getNewRobotTask :: Board -> Cell -> Board
getNewRobotTask board robot = newBoard where
    kids = filterByCellType Kid board
    dirt = filterByCellType Dirt board
    allDistancesAux = bfsDistance board [(robot,0)] [] []
    allDistances = bfsDistanceKidsPatch board kids allDistancesAux
    allRobots = filterByCellType Robot board
    nonTargetKids = getNonTargetCells board kids allRobots
    nonTargetDirts = getNonTargetCells board dirt allRobots
    reachableKids = filterByReachable nonTargetKids allDistances
    reachableDirts = filterByReachable nonTargetDirts allDistances

    actualTargetCellRow  = getCellTargetRow robot
    actualTargetCellCol  = getCellTargetCol robot
    -- to compare to the actual target

    betterDistance
        | actualTargetCellRow == -1 || actualTargetCellCol == -1 = 10000
        | otherwise = distanceAux where
            actualTarget = board !! actualTargetCellRow !! actualTargetCellCol
            tuple = getCellTuple actualTarget allDistances
            distanceAux | not (null tuple) = snd (head tuple)
                        | otherwise = 10000

    newBoard
     | getCellState robot == WithKid = board
     | getCellState robot == OnDirt && (actualTargetCellRow, actualTargetCellCol) == getPosition robot = board
     | not (null reachableKids) =
        let sortedTarget = sortOn snd reachableKids
            target    = fst (head sortedTarget)
            distance  = snd (head sortedTarget)
            rowTarget
                | distance < betterDistance = getCellRow target
                | otherwise = actualTargetCellRow
            colTarget
                | distance < betterDistance = getCellColumn target
                | otherwise = actualTargetCellCol
            row       = getCellRow robot
            col       = getCellColumn robot
            state     = getCellState robot
         in replaceCell (Robot, (row, col), state, (rowTarget, colTarget)) board

     | not (null reachableDirts) =
        let sortedTarget = sortOn snd reachableDirts

            actualTargetCellType
                | actualTargetCellRow == -1 || actualTargetCellCol == -1 = Empty
                | otherwise = getCellType (board !! actualTargetCellRow !! actualTargetCellCol)

            target = fst (head sortedTarget)
            distance  = snd (head sortedTarget)
            rowTarget
                | distance < betterDistance && actualTargetCellType /= Kid = getCellRow target
                | otherwise = actualTargetCellRow
            colTarget
                | distance < betterDistance && actualTargetCellType /= Kid = getCellColumn target
                | otherwise = actualTargetCellCol
            row       = getCellRow robot
            col       = getCellColumn robot
            state     = getCellState robot
         in replaceCell (Robot, (row, col), state, (rowTarget, colTarget)) board
     | otherwise = board



getNonTargetCells :: Board -> [Cell] -> [Cell] -> [Cell]
getNonTargetCells board cells robots = _getNonTargetCells board cells robots 0

_getNonTargetCells :: Board -> [Cell] -> [Cell] -> Int -> [Cell]
_getNonTargetCells board cells robots index
    | index == length cells = []
    | otherwise = nonTargetCells where
        cell = cells !! index
        isTarget = cell `elem` getAllRobotsTargets board robots
        nonTargetCells
            | isTarget = _getNonTargetCells board cells robots (index+1)
            | otherwise = cell : _getNonTargetCells board cells robots (index+1)


movesRobots :: Board -> Board
movesRobots board = newBoard where
    robots = filterByCellType Robot board
    boardWithTask = computeRobotTasks board robots
    newBoard = _moveRobots boardWithTask (filterByCellType Robot boardWithTask)


_moveRobots :: Board -> [Cell] -> Board
_moveRobots board [] = board
_moveRobots board (robot : t) = newBoard where
    targetRow = getCellTargetRow robot
    targetCol = getCellTargetCol robot
    robotState = getCellState robot
    nextBoard 
        -- si no tiene tarea asignada
        | targetRow == -1 && targetCol == -1 = board

        -- si no existe camino hasta ningun corral, tratar de obtener un path en cada turno, esperando al cambio 
        -- aleatorio en el ambiente
        | (targetRow, targetCol) == getPosition robot && (robotState == WithKid || robotState == OnDirtWithKid || robotState == OnCorralWithKid) =
            let (rowTarget, colTarget) = getCorralTarget board robot
             in replaceCell (Robot, (getCellRow robot, getCellColumn robot), robotState, (rowTarget, colTarget)) board  

        --si estas en tu casilla objetivo y te toca limpiar 
        | (targetRow, targetCol) == getPosition robot = cleanCell board robot

        | otherwise = newBoard1 where
            targetCell = board !! targetRow !! targetCol
            bfs = bfsLoop board targetCell [robot] [] []
            path = getPath robot targetCell bfs [targetCell]
            robotState = getCellState robot
            newBoard1
                |   robotState == WithKid || robotState == OnDirtWithKid || robotState == OnCorralWithKid =
                    let nextCell
                            | length path > 2 = path !! 2
                            | otherwise = path !! 1
                        newBoard3
                            | targetCell == nextCell = completeTask board robot targetCell
                            | otherwise = robotWalk board robot nextCell
                     in newBoard3

                | otherwise = newBoard2 where
                    nextCell = path !! 1
                    newBoard2
                        | targetCell == nextCell = completeTask board robot targetCell
                        | otherwise = robotWalk board robot nextCell
    newBoard = _moveRobots nextBoard t


cleanCell :: Board -> Cell -> Board
cleanCell board robot = newBoard where
    position = getPosition robot
    newBoard = replaceCell (Robot, position, Regular, (-1,-1)) board


robotWalk :: Board -> Cell -> Cell -> Board
robotWalk board robot targetCell = newBoard where
    robotRow = getCellRow robot
    robotCol = getCellColumn robot
    robotState = getCellState robot
    robotTargetRow = getCellTargetRow robot
    robotTargetCol = getCellTargetCol robot
    targetRow = getCellRow targetCell
    targetCol = getCellColumn targetCell
    targetCellType = getCellType targetCell
    targetState = getCellState targetCell

    newRobotState    = getNewRobotState targetCellType robotState
    oldRobotState    = getOldRobotState targetCellType robotState
    oldRobotCellType = getOldRobotCellType targetCellType robotState

    boardAux = replaceCell (oldRobotCellType, (robotRow, robotCol), oldRobotState, (-1, -1)) board
    newBoard = replaceCell (Robot, (targetRow, targetCol), newRobotState, (robotTargetRow, robotTargetCol)) boardAux



getNewRobotState :: CellType -> State -> State
getNewRobotState targetCellType robotState =
    let newRobotState
            | robotState == Regular && targetCellType == Empty = Regular
            | robotState == Regular && targetCellType == Kid = WithKid
            | robotState == Regular && targetCellType == Corral = OnCorral
            | robotState == Regular && targetCellType == Dirt = OnDirt
            | robotState == WithKid && targetCellType == Empty = WithKid
            | robotState == WithKid && targetCellType == Corral = OnCorralWithKid
            | robotState == WithKid && targetCellType == Dirt = OnDirtWithKid
            | robotState == OnCorral && targetCellType == Empty = Regular
            | robotState == OnCorral && targetCellType == Kid = WithKid
            | robotState == OnCorral && targetCellType == Corral = OnCorral
            | robotState == OnCorral && targetCellType == Dirt = OnDirt
            | robotState == OnDirt && targetCellType == Empty = Regular
            | robotState == OnDirt && targetCellType == Kid = WithKid
            | robotState == OnDirt && targetCellType == Corral = OnCorral
            | robotState == OnDirt && targetCellType == Dirt = OnDirt
            | robotState == OnCorralWithKid && targetCellType == Empty = Regular
            | robotState == OnCorralWithKid && targetCellType == Kid = WithKid
            | robotState == OnCorralWithKid && targetCellType == Corral = OnCorral
            | robotState == OnCorralWithKid && targetCellType == Dirt = OnDirt
            | robotState == OnDirtWithKid && targetCellType == Empty = WithKid
            | robotState == OnDirtWithKid && targetCellType == Corral = OnCorralWithKid
            | robotState == OnDirtWithKid && targetCellType == Dirt = OnDirtWithKid
            | otherwise = Regular
     in newRobotState


getOldRobotCellType :: CellType -> State -> CellType
getOldRobotCellType targetCellType robotState =
  let oldRobotCellType
        | robotState == Regular && targetCellType == Empty = Empty
        | robotState == Regular && targetCellType == Kid = Empty
        | robotState == Regular && targetCellType == Corral = Empty
        | robotState == Regular && targetCellType == Dirt = Empty

        | robotState == WithKid && targetCellType == Empty = Empty
        | robotState == WithKid && targetCellType == Corral = Empty
        | robotState == WithKid && targetCellType == Dirt = Empty

        | robotState == OnCorral && targetCellType == Empty = Corral
        | robotState == OnCorral && targetCellType == Kid = Corral
        | robotState == OnCorral && targetCellType == Corral = Corral
        | robotState == OnCorral && targetCellType == Dirt = Corral

        | robotState == OnDirt && targetCellType == Empty = Dirt
        | robotState == OnDirt && targetCellType == Kid = Dirt
        | robotState == OnDirt && targetCellType == Corral = Dirt
        | robotState == OnDirt && targetCellType == Dirt = Dirt

        | robotState == OnCorralWithKid && targetCellType == Empty = Corral
        | robotState == OnCorralWithKid && targetCellType == Kid = Corral
        | robotState == OnCorralWithKid && targetCellType == Corral = Corral
        | robotState == OnCorralWithKid && targetCellType == Dirt = Corral

        | robotState == OnDirtWithKid && targetCellType == Empty = Dirt
        | robotState == OnDirtWithKid && targetCellType == Kid = Dirt
        | robotState == OnDirtWithKid && targetCellType == Corral = Dirt
        | robotState == OnDirtWithKid && targetCellType == Dirt = Dirt
        | otherwise = Empty
   in oldRobotCellType

getOldRobotState :: CellType -> State -> State
getOldRobotState targetCellType robotState =
  let oldRobotState
        | robotState == Regular && targetCellType == Empty = Regular
        | robotState == Regular && targetCellType == Kid = Regular
        | robotState == Regular && targetCellType == Corral = Regular
        | robotState == Regular && targetCellType == Dirt = Regular

        | robotState == WithKid && targetCellType == Empty = Regular
        | robotState == WithKid && targetCellType == Corral = Regular
        | robotState == WithKid && targetCellType == Dirt = Regular

        | robotState == OnCorral && targetCellType == Empty = Regular
        | robotState == OnCorral && targetCellType == Kid = Regular
        | robotState == OnCorral && targetCellType == Corral = Regular
        | robotState == OnCorral && targetCellType == Dirt = Regular

        | robotState == OnDirt && targetCellType == Empty = Regular
        | robotState == OnDirt && targetCellType == Kid = Regular
        | robotState == OnDirt && targetCellType == Corral = Regular
        | robotState == OnDirt && targetCellType == Dirt = Regular

        | robotState == OnCorralWithKid && targetCellType == Empty = WithKid
        | robotState == OnCorralWithKid && targetCellType == Kid = WithKid
        | robotState == OnCorralWithKid && targetCellType == Corral = WithKid
        | robotState == OnCorralWithKid && targetCellType == Dirt = WithKid

        | robotState == OnDirtWithKid && targetCellType == Empty = Regular
        | robotState == OnDirtWithKid && targetCellType == Kid = Regular
        | robotState == OnDirtWithKid && targetCellType == Corral = Regular
        | robotState == OnDirtWithKid && targetCellType == Dirt = Regular
        | otherwise = Regular
   in oldRobotState


    -- newRobotState 
    --     | (robotState == WithKid || robotState == OnDirtWithKid) 
    --         && (targetCellType == Empty) = WithKid
    --     | (robotState == WithKid || robotState == OnDirtWithKid)
    --         && (targetCellType == Corral) = OnCorralWithKid
    --     | (robotState == WithKid || robotState == OnDirtWithKid)
    --         && (targetCellType == Dirt) = OnDirtWithKid
    --     | (robotState == OnCorralWithKid) && (targetCellType == Empty ) = Regular 
    --     | (robotState == OnCorralWithKid) && (targetCellType == Dirt ) = OnDirt
    --     | (robotState == OnCorralWithKid) && (targetCellType == Corral ) = OnCorral
    --     | robotState == OnCorral && targetCellType == Corral = OnCorral 
    --     | otherwise = targetState


    -- oldRobotCellType 
    --     | robotState == OnDirt = Dirt
    --     | robotState == Regular = Empty
    --     | robotState == OnCorral = Corral
    --     | robotState == OnCorralWithKid = Corral
    --     | robotState == OnDirtWithKid = Dirt
    --     | otherwise = Empty
    

    -- oldRobotState 
    --     | robotState == OnCorralWithKid = WithKid
    --     | robotState == OnDirtWithKid   = WithKid
    --     | otherwise = Regular


    -- oldRobotCellType
    --     | robotState == OnCorral = Corral
    --     | robotState == OnCorralWithKid = Corral
    --     | robotState == OnDirt = Dirt
    --     | robotState == OnDirtWithKid = Dirt
    --     | otherwise = Empty
    
    -- oldRobotState 
    --     | robotState == OnCorralWithKid = WithKid
    --     | robotState == WithKid = WithKid
    --     | robotState == OnDirtWithKid = WithKid
    --     | otherwise = robotState

    -- newRobotState
    --     | targetCellType == Corral = OnCorral
    --     | targetCellType == Corral && robotState == WithKid = OnCorralWithKid
    --     | targetCellType == Dirt = OnDirt
    --     | targetCellType == Dirt && robotState == WithKid = OnDirtWithKid
    --     | targetCellType == Empty && robotState == WithKid = WithKid
    --     | otherwise = Regular



completeTask :: Board -> Cell -> Cell -> Board
completeTask board robot targetCell = newBoard where
    robotRow = getCellRow robot
    robotCol = getCellColumn robot
    robotState = getCellState robot
    robotTargetRow = getCellTargetRow robot
    robotTargetCol = getCellTargetCol robot
    targetRow = getCellRow targetCell
    targetCol = getCellColumn targetCell
    targetCellType = getCellType targetCell

    newRobotState = getNewRobotState targetCellType robotState
    oldRobotState = getOldRobotState targetCellType robotState
    oldRobotCellType = getOldRobotCellType targetCellType robotState

    boardAux = replaceCell (oldRobotCellType, (robotRow, robotCol), oldRobotState, (-1, -1)) board
    newBoard
      | targetCellType == Corral = replaceCell (Robot, (targetRow, targetCol), newRobotState, (-1, -1)) boardAux
      | targetCellType == Dirt = replaceCell (Robot, (targetRow, targetCol), newRobotState, (targetRow, targetCol)) boardAux
      | targetCellType == Kid = replaceCell (Robot, (targetRow, targetCol), newRobotState, getCorralTarget boardAux robot) boardAux
      | otherwise = boardAux

    -- oldRobotCellType
    --     | robotState == OnDirt = Dirt
    --     | robotState == Regular = Empty
    --     | robotState == OnCorral = Corral
    --     | robotState == OnCorralWithKid = Corral
    --     | robotState == OnDirtWithKid = Dirt
    --     | otherwise = Empty

    -- oldRobotState
    --     | robotState == OnCorralWithKid = WithKid
    --     | robotState == OnDirtWithKid = WithKid
    --     | otherwise = Regular

    -- oldRobotCellType
    --   | robotState == OnCorral = Corral
    --   | robotState == OnCorralWithKid = Corral
    --   | robotState == OnDirt = Dirt
    --   | otherwise = Empty

    -- oldRobotState
    --   | robotState == OnCorralWithKid = WithKid
    --   | otherwise = robotState

    -- newRobotState
    --   | targetCellType == Corral = OnCorralWithKid
    --   | targetCellType == Dirt = OnDirt
    --   | targetCellType == Kid = WithKid
    --   | otherwise = Regular

    


getCorralTarget :: Board -> Cell -> (Int, Int)
getCorralTarget board robot = (targetRow, targetCol) where
    corrals = filterByCellType Corral board
    emptyCorrals = getEmptyCorrals corrals
    allDistances = bfsDistance board [(robot,0)] [] []
    allRobots = filterByCellType Robot board
    nonTargetCorrals = getNonTargetCells board emptyCorrals allRobots
    reachableCorrals = filterByReachable nonTargetCorrals allDistances

    corralTarget
        | not (null reachableCorrals) = fst (head (sortOn snd reachableCorrals))
        | otherwise = robot

    (targetRow, targetCol) = (getCellRow corralTarget, getCellColumn corralTarget)



getEmptyCorrals :: [Cell] -> [Cell]
getEmptyCorrals [] = []
getEmptyCorrals (corral : t)
    | getCellState corral == Regular = corral : getEmptyCorrals t
    | otherwise = getEmptyCorrals t


