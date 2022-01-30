module Types where


data CellType = Empty | Kid | Obstacle | Corral | Dirt
                | Robot | RobotKid | RobotDirt | RobotCorral
                | KidCorral | RobotKidCorral
                deriving (Eq)

type Position = (Int, Int)

type BoardCell = (CellType, Position)

type Board = [[BoardCell]]


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



-- BoardCell
getCellType :: BoardCell -> CellType
getCellType (cellType, position) = cellType

getPosition :: BoardCell -> Position
getPosition (cellType, position) = position

getX :: BoardCell -> Int
getX (celltype, position) = fst position

getY :: BoardCell -> Int
getY (celltype, position) = snd position

-- Board
getRowByIndex :: Board -> Int -> Int -> [BoardCell]
getRowByIndex board columns index = getRowByIndexAux board columns index 0

getRowByIndexAux :: Board -> Int -> Int -> Int -> [BoardCell]
getRowByIndexAux board columns index i
    | i == columns = []
    | otherwise = (board !! index !! i) : getRowByIndexAux board columns index (i+1)



