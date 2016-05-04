module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap()
import Graphics.Gloss.Data.Picture()
--import Control.Monad
--import Control.Monad.Trans
--import Control.Monad.List

{-}
-- | Play a game in a window.
play :: Display -- ^ Window to draw game in.
     -> Color   -- ^ Background color.
     -> Int     -- ^ Number of simulation steps per second of real time.
     -> a       -- ^ The initial game state.
     -> (a -> Picture)       -- ^ A function to render the world a picture.
     -> (Event -> a -> a)    -- ^ A function to handle input events.
     -> (Float -> a -> a)    -- ^ A function to step the world one iteration.
     -> IO ()
-}

window :: Display
window = InWindow "Sokoban" (1200, 800) (50, 30)

background :: Color
background = white

fps :: Int
fps = 60

update :: Float -> Game -> Game
update _ game = game
					--{time = liftIO getTicks}

--drawing :: Picture -> Picture
--drawing bitMap = pictures[bitMap, translate (-60) (-60) bitMap, translate 60 60 $ scale 2 2 bitMap]


type Coord = (Int, Int)

loadMapWithTextures :: MapTextures -> SokobanMap
loadMapWithTextures  _textures = SokobanMap
                                {size        = 50
                                ,walls       = []
                                ,targets     = []
                                ,boxes       = []
                                ,darkboxes   = []
                                ,spaces      = []
                                ,player      = (0,0)
                                ,textures    = _textures
                                ,previousMap = Nothing
                                }



mapSizes :: [(Int, Int)]
mapSizes = [ (9, 8),
             (8, 14),
             (10, 17)]

elemSize :: Int
elemSize = 60   --all element images are 60*60 pixels







makeMap :: String -> MapTextures -> Int -> SokobanMap
makeMap str mapTextures num = snd (foldl check ((0, 0), loadMapWithTextures mapTextures) str)
      where check ((i, j), mapStruct) symbol
                | (j == (snd (mapSizes !! num)) - 1) = ((i + 1, 0), addSymbol mapStruct symbol (i, j))
                | otherwise = ((i, j + 1), addSymbol mapStruct symbol (i, j))

addSymbol :: SokobanMap -> Char -> Coord -> SokobanMap
addSymbol mapStruct symbol coord =
    case symbol of
          '@' -> mapStruct{ player  = coord }
          'x' -> mapStruct{ boxes   = coord : boxes mapStruct }
          '#' -> mapStruct{ walls   = coord : walls mapStruct }
          '*' -> mapStruct{ targets = coord : targets mapStruct }
          '$' -> mapStruct{ spaces  = coord : spaces mapStruct }
          _ -> error (show symbol ++ " not recognized")

makeLevels :: [String] -> MapTextures -> Int -> [SokobanMap]
makeLevels [] _ _ = []
makeLevels (x : xs) mapTextures num = (makeMap x mapTextures num) : (makeLevels xs mapTextures (num + 1))
loadLevels :: String -> String -> [String]
loadLevels [] [] = []
loadLevels [] _ = []
loadLevels (x : xs) buf
      | x == '{' || x == '\n' = (loadLevels xs buf)
      | x == '}'              = buf : (loadLevels xs [])
      | otherwise             = loadLevels xs (buf ++ x : [])

makeGame :: Menu -> [SokobanMap] -> SokobanMap -> SokobanMap -> SokobanMap -> Int -> Int -> Float -> Game
makeGame menuStruct maps curr savedMap savedState num step_num s_time = Game {sokobanMaps = maps
                                                             ,currMap     = curr
                                                             ,backupMap   = savedMap
                                                             ,currNumber  = num
                                                             ,menu        = menuStruct
                                                             ,state       = 0
                                                             ,scaleAll    = 1.0
															 ,steps		  = step_num
															 ,labelSteps  = show_steps step_num
                                                             ,time        = s_time
															 }

makeMenu :: Picture -> Menu
makeMenu menuBg = Menu {menuBackground   = menuBg
                             ,labelHeader      = translate (-60)     150 $ scale 0.4 0.4 $ color white $ text "Menu"
                             ,labelScaleInc    = translate (-200)     80 $ scale 0.2 0.2 $ color white $ text "Press '+' to upscale"
                             ,labelScaleDec    = translate (-200)     30 $ scale 0.2 0.2 $ color white $ text "Press '-' to downscale"
                             ,labelRestart     = translate (-200)  (-20) $ scale 0.2 0.2 $ color white $ text "Press 'r' to restart level"
                             ,labelRestartGame = translate (-200)  (-70) $ scale 0.2 0.2 $ color white $ text "Press 'n' to restart game"
                             ,labelPrevState   = translate (-200) (-120) $ scale 0.2 0.2 $ color white $ text "Press 'p' to undo last step"
                             }



render :: Game -> Picture
-- the sequence of elements in the list (which is the argument for function pictures) shows
-- how elements will be rendered
render game
    | st   == 1 = pictures [pictures bg, menuBg, labelH, labelSI, labelSD, labelR, labelRG, labelPS]
    | otherwise = pictures (map pictures [bg, wallObjs, targetObjs, boxObjs, darkboxObjs, playerObj] ++ [(labelSteps game)])
     where
            st         = (state game)
            menuStruct = (menu game)
            menuBg     = (menuBackground menuStruct)
            labelH     = (labelHeader menuStruct)
            labelSI    = (labelScaleInc menuStruct)
            labelSD    = (labelScaleDec menuStruct)
            labelR     = (labelRestart menuStruct)
            labelRG    = (labelRestartGame menuStruct)
            labelPS    = (labelPrevState menuStruct)

            mapStruct  = (currMap game)
            num        = (currNumber game)
            sizeX      = fst (mapSizes !! num)
            sizeY      = snd (mapSizes !! num)
            scAll      = (scaleAll game)
            currSize   = (size mapStruct)
            resize     = (convert currSize) / (convert elemSize) * scAll  --koeff
            bg         = (bgTexture (textures mapStruct)) : []
            objsTexture= (textures mapStruct)

            wallObjs   = makeObjs (walls mapStruct)          (wallTexture    objsTexture) sizeX sizeY resize
            targetObjs = makeObjs (targets mapStruct)        (targetTexture  objsTexture) sizeX sizeY resize
            boxObjs    = makeObjs (boxes mapStruct)          (boxTexture     objsTexture) sizeX sizeY resize
            darkboxObjs= makeObjs (darkboxes mapStruct)      (darkboxTexture objsTexture) sizeX sizeY resize
            playerObj  = makeObjs ((player mapStruct) : [])  (playerTexture  objsTexture) sizeX sizeY resize

show_steps :: Int -> Picture
show_steps step_num = translate (-30)    230 $ scale 0.4 0.4 $ color white $ text (show step_num)

convert :: Int -> Float
convert x = fromIntegral (x :: Int) :: Float

makeObjs :: [Coord] -> Picture -> Int -> Int -> Float -> [Picture]
makeObjs [] _ _ _ _= []
makeObjs ((i, j) : xs) pic sizeX sizeY koeff = (translate  x y $ scale koeff koeff pic) : makeObjs xs pic sizeX sizeY koeff
          where
            x = (convert (-(sizeY `div` 2) * elemSize + j * elemSize)) * koeff
            y = (convert ( (sizeX `div` 2) * elemSize - i * elemSize)) * koeff







showMenu :: Game -> Game
showMenu game
  | st == 0 = game {state = 1}
  | otherwise = game {state = 0}
  where st = (state game)

loadNextLevel :: Game -> Game
loadNextLevel game = game {currMap = newMap, backupMap = newMap, currNumber = num, steps = 0, time = 0}
                      where
                        maps = (sokobanMaps game)
                        num = ((currNumber game) + 1) `mod` (length maps)
                        newMap = maps !! num

startFromBegin :: Game -> Game
startFromBegin game = game {currMap     = firstMap
                           ,backupMap   = firstMap
                           ,currNumber  = 0
					       ,steps       = 0
						   ,labelSteps  = show_steps 0
						   ,time        = 0
                           }
                      where
                        maps = (sokobanMaps game)
                        firstMap = maps !! 0

reloadLevel :: Game -> Game
reloadLevel game = game {currMap = savedMap, steps = 0 ,labelSteps  = show_steps 0, time = 0}
                   where
                     savedMap = (backupMap game)

makeStepBack :: Game -> Game
makeStepBack game = game {currMap = savedState, steps = (steps game) - 1 , labelSteps  = show_steps ((steps game) - 1)}
                    where
                      savedState = case (previousMap (currMap game)) of
                        Nothing -> (currMap game)
                        Just state -> state

scaleIn :: Game -> Game
scaleIn game = game {scaleAll = scAll + 0.1}
               where
                 scAll = (scaleAll game)

scaleOut :: Game -> Game
scaleOut game = game {scaleAll = scAll - 0.1}
               where
                 scAll = (scaleAll game)

makeMove :: (Int, Int) -> Game -> Game
makeMove (di, dj) game = action game (i + di, j + dj) (i + 2*di, j + 2*dj)
                         where
                           mapStruct = (currMap game)
                           i = fst (player mapStruct)
                           j = snd (player mapStruct)

moveDown :: Game -> Game
moveDown = makeMove (1, 0)

moveUp :: Game -> Game
moveUp = makeMove (-1, 0)

moveLeft :: Game -> Game
moveLeft = makeMove (0, -1)

moveRight :: Game -> Game
moveRight = makeMove (0, 1)

-- | Respond to key events.
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char c) Down _ _) game = case c of
  'm' -> showMenu game
  '.' -> loadNextLevel game
  'n' -> startFromBegin game
  'r' -> reloadLevel game
  'p' -> makeStepBack game
  '=' -> scaleIn game
  '-' -> scaleOut game
  's' -> moveDown game
  'w' -> moveUp game
  'a' -> moveLeft game
  'd' -> moveRight game
  -- Do nothing for all other events.
  _ -> game
handleKeys _ game = game

checkWin :: Game -> Game
checkWin game
        | length boxesCoords == 0 = game{currNumber  = nextNumber
                                        ,currMap     = newMap
                                        ,backupMap   = newMap
                                        }
        | otherwise = game
        where
          num         = (currNumber game)
          mapStruct   = (currMap game)
          maps        = (sokobanMaps game)
          boxesCoords = (boxes mapStruct)
          nextNumber  = (num + 1) `mod` (length maps)

          newMap      = maps !! nextNumber

action :: Game -> Coord -> Coord -> Game
action game newCoord followCoord
        | (isObject newCoord wallsCoords) ||
          ((isObject newCoord boxesCoords) || (isObject newCoord darkboxesCoords)) && ((isObject followCoord boxesCoords) || (isObject followCoord darkboxesCoords)) ||
          ((isObject newCoord boxesCoords) || (isObject newCoord darkboxesCoords)) && (isObject followCoord wallsCoords)  = game

        | (isObject newCoord boxesCoords) && (not (isObject followCoord wallsCoords)) =
            checkWin game {currMap = mapStruct {player = newCoord
                                                ,boxes = if (isObject followCoord targetsCoords) then (deleteBox newCoord boxesCoords)
                                                         else (moveBox newCoord followCoord boxesCoords)
                                                ,darkboxes = if (isObject followCoord targetsCoords) then followCoord : darkboxesCoords
                                                             else darkboxesCoords
                                                ,previousMap = Just mapStruct
                                                }
												,steps = stepNumber + 1
												,labelSteps  = show_steps (stepNumber + 1)
												
                          }

        | (isObject newCoord darkboxesCoords) && (not (isObject followCoord wallsCoords)) =
            checkWin game {currMap = mapStruct {player = newCoord
                                               ,boxes = if (isObject followCoord targetsCoords) then boxesCoords
                                                        else followCoord : boxesCoords
                                               ,darkboxes = if (isObject followCoord targetsCoords) then (moveBox newCoord followCoord darkboxesCoords)
                                                            else (deleteBox newCoord darkboxesCoords)
                                               ,previousMap = Just mapStruct	
                                               }
											   ,steps = stepNumber + 1
											   ,labelSteps  = show_steps (stepNumber + 1)
                          }



        | otherwise = game {currMap = mapStruct {player = newCoord
                                                ,previousMap = Just mapStruct
                                                }
												,steps = stepNumber + 1
												,labelSteps  = show_steps (stepNumber + 1)
                           }
        where
           mapStruct        = (currMap game)
           wallsCoords      = (walls   mapStruct)
           targetsCoords    = (targets mapStruct)
           boxesCoords      = (boxes   mapStruct)
           stepNumber       = (steps   game)
           darkboxesCoords  = (darkboxes mapStruct)


isObject :: Coord -> [Coord] -> Bool
isObject (i, j) objects = foldr (\(a, b) acc -> (i == a && j == b) || acc) False objects

moveBox :: Coord -> Coord -> [Coord] -> [Coord]
moveBox (i, j) (newI, newJ) _boxes = map (\(a, b) -> if (i == a && j == b) then (newI, newJ) else (a, b)) _boxes

deleteBox :: Coord -> [Coord] -> [Coord]
deleteBox (i, j) _boxes = filter (\(a, b) -> not (i == a && j == b)) _boxes






main :: IO()
main = do
     src <- readFile "levels.txt"
     levelsData <- return (loadLevels src [])   -- converts any object to IO object

     box     <- loadBMP "./images/box.bmp"
     darkbox <- loadBMP "./images/darkbox.bmp"
     wall    <- loadBMP "./images/wall.bmp"
     space   <- loadBMP "./images/space.bmp"
     target  <- loadBMP "./images/target.bmp"
     _player  <- loadBMP "./images/player.bmp"
     bg      <- loadBMP "./images/background3.bmp"

     levels  <- return (makeLevels levelsData
                                  MapTextures {boxTexture = box
                                               ,darkboxTexture = darkbox
                                               ,wallTexture = wall
                                               ,spaceTexture = space
                                               ,targetTexture = target
                                               ,playerTexture = _player
                                               ,bgTexture = bg}
                        0)

     menuBg <- loadBMP "./images/menuBg.bmp"

     _menu <- return (makeMenu menuBg)
     game <- return (makeGame _menu levels (levels !! 0) (levels !! 0) (levels !! 0) 0 0 0)
     play window background fps game render handleKeys update

     return ()
