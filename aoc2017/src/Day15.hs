{-# LANGUAGE BinaryLiterals #-}
module Day15 where

import Data.Bits ((.&.))
import Data.Word (Word32)

generator :: Int -> Int -> [Word32]
generator factor prev =
  let val = (prev * factor) `mod` 2147483647
  in fromIntegral val : generator factor val

generatorA = generator 16807
generatorB = generator 48271

generators (initA, initB) = zip (generatorA initA) (generatorB initB)

mask :: Word32
mask = 0b00000000000000001111111111111111

judgePair :: Word32 -> Word32 -> Bool
judgePair a b =
  a .&. mask == b .&. mask

judge :: Int -> (Int, Int) -> Int
judge n inits =
  length .
  filter (uncurry judgePair) .
  take n .
  generators $ inits
