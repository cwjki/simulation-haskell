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

