module Types where


data CellType = Empty | Kid | Obstacle | Corral | Dirt
                | Robot | RobotKid | RobotDirt | RobotCorral
                | KidCorral | RobotKidCorral
                deriving (Eq)

type Position = (Int, Int)

type Cell = (CellType, Position)

type Board = [[Cell]]


instance Show CellType where
    show Empty          = "[   ]"
    show Kid            = "[K  ]"
    show Obstacle       = "[O  ]"
    show Corral         = "[C  ]"
    show Dirt           = "[D  ]"
    show Robot          = "[R  ]"
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



-- Board

-- return a list of cells with the desired cellType
filterByCellType :: CellType -> Board -> [Cell]
filterByCellType cellType board = filterByCellTypeRow cellType board 0 (length (head board))

filterByCellTypeRow :: CellType -> Board -> Int -> Int -> [Cell]
filterByCellTypeRow cellType board row rowLength
    | row == rowLength = []
    | otherwise = filterByCellTypeColumn cellType board row 0 (length board) ++ filterByCellTypeRow cellType board (row+1) rowLength

filterByCellTypeColumn :: CellType -> Board -> Int -> Int -> Int -> [Cell]
filterByCellTypeColumn cellType board row column columnLength
    | column == columnLength = []
    | getCellType (getCell board row column) == cellType = getCell board row column : filterByCellTypeColumn cellType board row (column+1) columnLength
    | otherwise = filterByCellTypeColumn cellType board row (column+1) columnLength




-- return the i-th row 
getRowByIndex :: Board -> Int -> Int -> [Cell]
getRowByIndex board columns index = getRowByIndexAux board columns index 0

getRowByIndexAux :: Board -> Int -> Int -> Int -> [Cell]
getRowByIndexAux board columns index i
    | i == columns = []
    | otherwise = (board !! index !! i)
    : getRowByIndexAux board columns index (i + 1)



