-- stack --resolver lts --install-ghc runghc --package gloss --package random
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Snake
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Debug.Trace

data GUIState
  = Game
    { gameState :: GameState
    }
  | LevelSelect
    { levels :: [(Grid, Snake, Dir)]
    , idx :: Int
    }

changeLevel levelSelect@LevelSelect { idx, levels } offs
  = levelSelect { idx = (idx + offs + count) `mod` count }
  where
    count = length levels

initialGuiState :: GUIState
initialGuiState = LevelSelect levels 0
  where
    levels = [
        (readLevel $ [
          "#####",
          "#   #",
          "#   #",
          "#   #",
          "#####"
        ], [(1, 1)], DOWN),
        (readLevel $ [
          "##########",
          "#        #",
          "#  ####  #",
          "#        #",
          "##########"
        ], [(1, 1), (1, 2)], DOWN)
      ]
    readLevel :: [String] -> Grid
    readLevel = map (map readCell)
    readCell ' ' = FREE
    readCell '#' = WALL

toFloat (x, y) = (fromIntegral x, fromIntegral y)

windowSize = (640, 480)

window :: Display
window = InWindow "Haskell Snake Game" windowSize (100, 100)

background :: Color
background = white

render :: GUIState -> Picture
render LevelSelect { levels, idx } = renderGrid grid snake []
  where
    (grid, snake, food) = levels !! idx
render Game { gameState } = renderGame gameState
  where
    renderGame GameState { snake, food, level }
      = renderGrid level snake [food]

renderGrid :: Grid -> Snake -> [Pos] -> Picture
renderGrid grid snake food
  = pictures $
    map (convertToPicture black) wallCoords ++
    map (convertToPicture blue) swapCoords ++
    map (convertToPicture red) (map swapCoords food)
  where
    wallCoords :: [Pos]
    wallCoords = map swapCoords $ concat $ map toCoords $ zip [0 ..] $ map (elemIndices WALL) grid
    toCoords (x, ys) = map ((,)x) ys
    swapCoords (x, y) = (y, x)
  
    convertToPicture :: Color -> (Int, Int) -> Picture
    convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) cellSizeF offsetF
    fillRectangle color' (tx, ty) (w, h) (ow, oh)
      = color color' $
        scale 1 (-1) $
        translate (tx * w + ow) (ty * h + oh) $
        rectangleSolid w h

    (winWidth, winHeight) = windowSize
    winW2 = winWidth `div` 2
    winH2 = winHeight `div` 2
  
    width = length $ head grid
    height = length grid
    
    wView = winWidth `div` width
    hView = winHeight `div` height
  
    cellSize = min wView hView
    offsetX = ((-cellSize) * (width - 1)) `div` 2
    offsetY = ((-cellSize) * (height - 1)) `div` 2

    cellSizeF = toFloat (cellSize, cellSize)
    offsetF = toFloat (offsetX, offsetY)

update :: Float -> GUIState -> GUIState
update _ guiState@Game { gameState }
  = if isRunning gameState
    then Game { gameState = move gameState }
    else guiState

update _ guiState = guiState

handleKeys :: Event -> GUIState -> GUIState

-- gameplay
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) guiState@Game { gameState }
  = guiState { gameState = changeDir gameState LEFT }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) guiState@Game { gameState }
  = guiState { gameState = changeDir gameState RIGHT } 
handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) guiState@Game { gameState }
  = guiState { gameState = changeDir gameState UP } 
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) guiState@Game { gameState }
  = guiState { gameState = changeDir gameState DOWN }
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) guiState@Game { gameState }
  = if not $ isRunning gameState
    then initialGuiState
    else guiState

-- level select
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) guiState@LevelSelect{}
  = changeLevel guiState 1
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) guiState@LevelSelect{}
  = changeLevel guiState (-1)
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) guiState@LevelSelect { idx, levels }
  = Game { gameState = initialGameState grid snake dir }
  where
    (grid, snake, dir) = levels !! idx

handleKeys _ gameState = gameState

main :: IO ()
main = play window background 1 initialGuiState render handleKeys update

