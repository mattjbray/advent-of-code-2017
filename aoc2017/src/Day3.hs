{-# LANGUAGE TemplateHaskell #-}
module Day3 where

import Control.Lens
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Position = Position
  { _ups :: Int
  , _rights :: Int
  } deriving (Show, Eq, Ord)

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

data Step = Step
  { _currentPos :: Position
  , _minPos :: Position
  , _maxPos :: Position
  , _currentDir :: Direction
  } deriving (Show)

makeLenses ''Step

initStep :: Step
initStep =
  Step
  { _currentPos = Position 0 0
  , _minPos = Position 0 0
  , _maxPos = Position 0 0
  , _currentDir = DRight
  }

nextStep :: Step -> Step
nextStep step =
  case step ^. currentDir of
    DRight ->
      let step' = step & (currentPos . rights) +~ 1
      in if step' ^. currentPos . rights > step' ^. maxPos . rights
           then let step'' = step' & (maxPos . rights) +~ 1
                in step'' & currentDir %~ nextDir
           else step'
    DUp ->
      let step' = step & (currentPos . ups) +~ 1
      in if step' ^. currentPos . ups > step' ^. maxPos . ups
           then let step'' = step' & (maxPos . ups) +~ 1
                in step'' & currentDir %~ nextDir
           else step'
    DLeft ->
      let step' = step & (currentPos . rights) -~ 1
      in if step' ^. currentPos . rights < step' ^. minPos . rights
           then let step'' = step' & (minPos . rights) -~ 1
                in step'' & currentDir %~ nextDir
           else step'
    DDown ->
      let step' = step & (currentPos . ups) -~ 1
      in if step' ^. currentPos . ups < step' ^. minPos . ups
           then let step'' = step' & (minPos . ups) -~ 1
                in step'' & currentDir %~ nextDir
           else step'

squarePositions :: [Position]
squarePositions = map _currentPos . iterate nextStep $ initStep

manhattanDistance :: Int -> Int
manhattanDistance n =
  let finalPos = last . take n $ squarePositions
  in finalPos ^. ups . to abs + finalPos ^. rights . to abs

neighbours :: Position -> [Position]
neighbours p =
  [ p & ups +~ 1 & rights -~ 1, p & ups +~ 1, p & ups +~ 1 & rights +~ 1
  , p & ups +~ 0 & rights -~ 1,               p & ups +~ 0 & rights +~ 1
  , p & ups -~ 1 & rights -~ 1, p & ups -~ 1, p & ups -~ 1 & rights +~ 1
  ]

squareValues :: [Int]
squareValues =
  1 : go (nextStep initStep) (M.fromList [(initStep ^. currentPos, 1)])
  where
    go :: Step -> M.Map Position Int -> [Int]
    go step valuesByPosition =
      let val =
            sum .
            mapMaybe (\p -> M.lookup p valuesByPosition) .
            neighbours . view currentPos $
            step
      in val :
         go (nextStep step) (M.insert (step ^. currentPos) val valuesByPosition)
