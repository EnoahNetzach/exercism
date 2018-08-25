module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

import Data.List

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { bearing :: Bearing, coordinates :: (Integer, Integer) }

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coord = Robot { bearing = direction, coordinates = coord }

move :: Bearing -> (Integer, Integer) -> (Integer, Integer)
move North (x, y) = (x, y + 1)
move East (x, y) = (x + 1, y)
move South (x, y) = (x, y - 1)
move West (x, y) = (x - 1, y)

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

step :: Robot -> Char -> Robot
step Robot { bearing = b, coordinates = c } 'A' = Robot { bearing = b, coordinates = move b c }
step Robot { bearing = b, coordinates = c } 'L' = Robot { bearing = turnLeft b, coordinates = c }
step Robot { bearing = b, coordinates = c } 'R' = Robot { bearing = turnRight b, coordinates = c }

simulate :: Robot -> String -> Robot
simulate = foldl step
