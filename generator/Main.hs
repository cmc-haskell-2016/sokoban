module Main where

import Lib
import Data.List
import Debug.Trace

max_step_count = 38

debug = flip trace

data MapState = MapState {
    size :: Int,
    walls :: [Coord],
    targets :: [Coord],
    boxes :: [Coord],
    darkboxes :: [Coord],
    spaces :: [Coord]
} deriving (Show, Eq, Ord)

data GameState = GameState {
    mapState :: MapState,
    player :: Coord
} deriving (Show, Eq, Ord)

data Movement = LEFT | RIGHT | UP | DOWN

contains :: (Eq a) => [a] -> a -> Bool
contains list element = any (\x -> x == element) list

isFree :: MapState -> Coord -> Bool
isFree (MapState _ _ targets _ _ spaces) cell = contains spaces cell

isBox :: MapState -> Coord -> Bool
isBox (MapState _ _ _ boxes darkboxes _) cell = contains boxes cell || contains darkboxes cell

moveCoord :: Coord -> Movement -> Coord
moveCoord (x, y) LEFT = (x - 1, y)
moveCoord (x, y) RIGHT = (x + 1, y)
moveCoord (x, y) DOWN = (x, y - 1)
moveCoord (x, y) UP = (x, y + 1)

canMove :: GameState -> Movement -> Bool
canMove (GameState mapState player) direction = isFree mapState (moveCoord player direction) || 
                        (isBox mapState (moveCoord player direction) && isFree mapState (moveCoord (moveCoord player direction) direction))

removeBox :: MapState -> Coord -> MapState
removeBox (MapState size walls targets boxes darkboxes spaces) cell 
    | contains darkboxes cell = (MapState size walls targets boxes (delete cell darkboxes) (cell : spaces))
    | contains boxes cell = (MapState size walls targets (delete cell boxes) darkboxes (cell : spaces))
    | otherwise = error "Box does not even exist!"

addBox :: MapState -> Coord -> MapState
addBox (MapState size walls targets boxes darkboxes spaces) cell 
    | contains targets cell = (MapState size walls targets boxes (cell : darkboxes) (delete cell spaces))
    | otherwise = (MapState size walls targets (cell : boxes) darkboxes (delete cell spaces))

moveBox :: MapState -> Coord -> Coord -> MapState
moveBox mapState currentCell newCell = addBox (removeBox mapState currentCell) newCell

move :: GameState -> Movement -> GameState
move (GameState mapState player) direction
    | not (canMove (GameState mapState player) direction) = error "Cannot move!"
    | isFree mapState newCell = (GameState mapState newCell)
    | isBox mapState newCell = (GameState (moveBox mapState newCell newBoxCell) newCell)
    | otherwise = error "The new cell is not free neither box!"
    where newCell = moveCoord player direction
          newBoxCell = moveCoord newCell direction

isFinishState :: GameState -> Bool
isFinishState (GameState (MapState _ _ _ boxes _ _) _) = (length boxes) == 0

tryGo :: GameState -> Movement -> [GameState]
tryGo gameState movement
    | canMove gameState movement = [move gameState movement]
    | otherwise = []

goEverywhere :: GameState -> [GameState]
goEverywhere gameState = tryGo gameState LEFT ++ tryGo gameState RIGHT ++ tryGo gameState DOWN ++ tryGo gameState UP

makeUnique :: [GameState] -> [GameState]
makeUnique = map head . group . sort

internalStepsCount :: [GameState] -> Integer -> Integer
internalStepsCount list stepsCount 
    | stepsCount >= max_step_count = -1
    | any isFinishState list = stepsCount
    | otherwise = internalStepsCount (makeUnique (concat (map (\x -> goEverywhere x) list))) (stepsCount + 1) `debug` ((show stepsCount) ++ " : " ++(show (length list)))

main :: IO()
main = do
    (print (internalStepsCount [gameState] 0))
    where playerPos = (2, 2)
          gameState = (GameState (MapState 9 [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (0, 1), (1, 1), (2, 1), (6, 1), (7, 1), (0, 2), (6, 2), (7, 2), (0, 3), (1, 3), (2, 3), (6, 3), (7, 3), (0, 4), (2, 4), (3, 4), (6, 4), (7, 4), (0, 5), (2, 5), (6, 5), (7, 5), (0, 6), (7, 6), (0, 7), (7, 7), (0, 8), (1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (6, 8), (7, 8)] [(1, 2), (5, 3), (1, 4), (4, 5), (6, 6), (4, 7)] [(3, 2), (4, 3), (4, 4), (1, 6), (4, 6), (5, 6)] [] [(3, 1), (4, 1), (5, 1), (1, 2), (4, 2), (5, 2), (3, 3), (5, 3), (1, 4), (5, 4), (1, 5), (3, 5), (4, 5), (5, 5), (2, 6), (3, 6), (6, 6), (1, 7), (2, 7), (3, 7), (4, 7), (5, 7), (6, 7)]) playerPos)
