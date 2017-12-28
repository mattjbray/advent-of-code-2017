module Day5 where

import           Control.Monad.ST    (ST, runST)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MV

jump :: (Int -> Int) -> Int -> STVector s Int -> ST s Int
jump f i v = do
  inst <- MV.read v i
  MV.modify v f i
  return (i + inst)

part1 :: Int -> Int
part1 = (+) 1

part2 :: Int -> Int
part2 inst =
  if inst >= 3
    then inst - 1
    else inst + 1

go :: (Int -> Int) -> Int -> STVector s Int -> Int -> ST s Int
go f i v nSteps =
  if i >= MV.length v then
    return nSteps
  else do
    i' <- jump f i v
    go f i' v (nSteps + 1)

steps :: (Int -> Int) -> Vector Int -> Int
steps f v =
  runST $ do
    mv <- V.thaw v
    go f 0 mv 0

getInstructions :: IO (Vector Int)
getInstructions =
  V.fromList . map read . lines <$> readFile "../resources/day_5.txt"
