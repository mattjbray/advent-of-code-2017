module Day15 where

import Data.Word (Word16)

generator :: Int -> Int -> Int
generator factor prev =
  (prev * factor) `mod` 2147483647

generatorA = generator 16807
generatorB = generator 48271

judgePair :: (Int, Int) -> Bool
judgePair (a, b) =
  (fromIntegral a :: Word16) == (fromIntegral b :: Word16)

judge :: (Int, Int, Int) -> (Int, Int, Int)
judge (count, prevA, prevB) =
  let a = generatorA prevA
      b = generatorB prevB
      count' =
        if judgePair (a, b) then
          count + 1
        else
          count
  in
    (count', a, b)
