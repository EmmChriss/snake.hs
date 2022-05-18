module Snake where

import Data.Map as Map
import System.Random

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)
type Pos = (Int, Int)
type Snake = [Pos]

data Cell = FREE | WALL deriving (Eq)
type Grid = [[Cell]]

data State = RUNNING | WIN | LOST deriving (Eq)

data GameState = GameState
  { snake :: Snake
  , dir :: Direction
  , food :: Pos
  , level :: Grid
  , rand :: StdGen
  , state :: State
  , remainingCells :: Int
  }

getCell :: Grid -> Pos -> Cell
getCell grid (x, y) = grid !! y !! x

isRunning :: GameState -> Bool
isRunning GameState { state }
  | state == RUNNING = True
  | otherwise = False

initialGameState :: Grid -> Snake -> Dir -> GameState
initialGameState level snake dir
  = generateFood $ GameState
    { snake = snake
    , dir = dir
    , food = (0, 0) -- overwritten by generateFood
    , level = level
    , rand = mkStdGen 100
    , state = RUNNING
    , remainingCells = countFree - length snake
    }
  where
    countFree = sum $ map
      (\row -> sum $ map
        (\cell ->
          if cell == FREE
          then 1
          else 0
          )
        ) level

dirToVec :: Dir -> Pos
dirToVec UP = (0, (-1))
dirToVec DOWN = (0, 1)
dirToVec LEFT = ((-1), 0)
dirToVec RIGHT = (1, 0)

move :: GameState -> GameState
move gameState@GameState { snake, dir, food, level }
  | not $ isRunning gameState = gameState
  | getCell level newHead == WALL || newHead `elem` snake
    = gameState { state = LOST }
  | food == newHead = generateFood $ gameState { snake = longSnake }
  | otherwise = gameState { snake = init longSnake }
  where
    posAdd :: Pos -> Pos -> Pos
    posAdd (x, y) (a, b) = (x + a, y + b)

    width = length $ head level
    height = length level
    posWrap (x, y) = ((x + width) `mod` width, ((y + height) `mod` height))

    currHead = head snake
    newHead = posWrap $ posAdd currHead $ dirToVec dir
    longSnake = newHead : snake

generateFood :: GameState -> GameState
generateFood gameState@GameState { rand, remainingCells }
  | remainingCells == 0 = gameState
  | otherwise = gameState
  where
    width = length $ head level
    height = length level
    
    mapFst f (a, b) = (f a, b)
    mapSnd f (a, b) = (a, f b)
    mapBoth f (a, b) = (f a, f b)
    rands = map (map) map (mapBoth abs) $ drop 1 $ iterate (random.snd) ((0, 0), rand)
    
    (randW, rand1) = map (mapFst (`mod` width)) rands
    randsH = filter (<height.fst) $ snd rands
    randPairs = filter validPos $ zip randsW randsH
    validPos pos = 

changeDir :: GameState -> Direction -> GameState
changeDir gameState dir = gameState { dir = dir }
