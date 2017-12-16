{-# LANGUAGE TemplateHaskell #-}
module Day3 where

import Control.Lens

data Position = Position
  { _ups :: Int
  , _rights :: Int
  } deriving (Show)

makeLenses ''Position

data Direction
  = DRight
  | DUp
  | DLeft
  | DDown
  deriving (Show)

nextDir :: Direction -> Direction
nextDir DRight = DUp
nextDir DUp = DLeft
nextDir DLeft = DDown
nextDir DDown = DRight

data Acc = Acc
  { _currentPos :: Position
  , _minPos :: Position
  , _maxPos :: Position
  , _currentDir :: Direction
  } deriving (Show)

makeLenses ''Acc

initAcc :: Acc
initAcc =
  Acc
  { _currentPos = Position 0 0
  , _minPos = Position 0 0
  , _maxPos = Position 0 0
  , _currentDir = DRight
  }

acc = initAcc

nextPos :: Acc -> Acc
nextPos acc =
  case acc ^. currentDir of
    DRight ->
      let acc' = acc & (currentPos . rights) +~ 1
      in if acc' ^. currentPos . rights > acc' ^. maxPos . rights
           then let acc'' = acc' & (maxPos . rights) +~ 1
                in acc'' & currentDir %~ nextDir
           else acc'
    DUp ->
      let acc' = acc & (currentPos . ups) +~ 1
      in if acc' ^. currentPos . ups > acc' ^. maxPos . ups
           then let acc'' = acc' & (maxPos . ups) +~ 1
                in acc'' & currentDir %~ nextDir
           else acc'
    DLeft ->
      let acc' = acc & (currentPos . rights) -~ 1
      in if acc' ^. currentPos . rights < acc' ^. minPos . rights
           then let acc'' = acc' & (minPos . rights) -~ 1
                in acc'' & currentDir %~ nextDir
           else acc'
    DDown ->
      let acc' = acc & (currentPos . ups) -~ 1
      in if acc' ^. currentPos . ups < acc' ^. minPos . ups
           then let acc'' = acc' & (minPos . ups) -~ 1
                in acc'' & currentDir %~ nextDir
           else acc'

posAfterSteps :: Int -> [Position]
posAfterSteps n =
  map _currentPos . take n . iterate nextPos $ initAcc

manhattanDistance :: Int -> Int
manhattanDistance n =
  let finalPos =
        last $ posAfterSteps n
      in
    finalPos ^. ups . to abs + finalPos ^. rights . to abs

part1 = manhattanDistance 265149
