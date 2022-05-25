{-# LANGUAGE NamedFieldPuns #-}

module Snake where

import Data.List
import System.Random

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)
type Pos = (Int, Int)
type Snake = [Pos]

data Cell = FREE | WALL deriving (Eq)
type Grid = [[Cell]]

data State = RUNNING | WIN | LOST deriving (Eq)

data GameState = GameState
  { snake :: Snake
  , dir :: Dir
  , food :: Pos
  , level :: Grid
  , rand :: StdGen
  , state :: State
  }

getCell :: Grid -> Pos -> Cell
getCell grid (x, y) = grid !! x !! y

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
    }

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
generateFood gameState@GameState { rand, level, snake }
  | len == 0 = gameState { rand = rand1, food = ((-1), (-1)), state = WIN }
  | otherwise = gameState { rand = rand1, food = coord }
  where
    freeGridCoords :: [Pos]
    freeGridCoords = concat $ map toCoords $ zip [0 ..] $ map (elemIndices FREE) level
    toCoords (x, ys) = map ((,)x) ys

    possibleCoords :: [Pos]
    possibleCoords = filter (not.(flip elem snake)) freeGridCoords
    len = length possibleCoords
    
    (idx, rand1) = randomR (0, len - 1) rand
    coord = possibleCoords !! idx

changeDir :: GameState -> Dir -> GameState
changeDir gameState dir = gameState { dir = dir }
