{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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

solve2 :: Int -> Int
solve2 stepSize =
  go 0 0 1
  where
    go currentPosition lastValAfterZero nextValue =
      let len = nextValue
          newPosition =
            ((currentPosition + stepSize) `mod` len) + 1
          newValAfterZero =
            if newPosition == 1 then nextValue else lastValAfterZero
      in if nextValue == 50 * 10 ^ 6 then
           newValAfterZero
         else
           go newPosition newValAfterZero (nextValue + 1)
