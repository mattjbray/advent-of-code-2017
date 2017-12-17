module Day5 where

import Data.Vector ((!), (//), Vector)
import qualified Data.Vector as V

jump :: (Int -> Int) -> (Int, Vector Int) -> (Int, Vector Int)
jump f (i, v) =
  let inst = v ! i
      v' = v // [(i, f inst)]
  in (i + inst, v')

part1 :: Int -> Int
part1 = (+) 1

part2 :: Int -> Int
part2 inst =
  if inst >= 3
    then inst - 1
    else inst + 1

steps :: (Int -> Int) -> Vector Int -> Int
steps f v =
  let len = length v in
  length . takeWhile (\(i, v) -> i < len) . iterate (jump f) $ (0, v)

getInstructions :: IO (Vector Int)
getInstructions =
  V.fromList . map read . lines <$> readFile "../resources/day_5.txt"
