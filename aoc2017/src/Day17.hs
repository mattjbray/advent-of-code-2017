module Day17 where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

insert :: Int -> Int -> Seq Int -> Int -> (Int, Seq Int)
insert stepSize currentPosition buffer newValue =
  let newPosition = ((currentPosition + stepSize) `mod` Seq.length buffer) + 1
      newBuffer = Seq.insertAt newPosition newValue buffer
  in (newPosition, newBuffer)

spinlock :: Int -> [(Int, Seq Int)]
spinlock stepSize =
  (0, Seq.singleton 0) : go 0 (Seq.singleton 0) 1
  where go currentPosition buffer nextValue =
          let (newPosition, newBuffer) = insert stepSize currentPosition buffer nextValue
          in (newPosition, newBuffer) : go newPosition newBuffer (nextValue + 1)

solve :: Int -> Int
solve stepSize =
  let (index, buffer) = spinlock stepSize !! 2017
  in Seq.index buffer (index + 1)
