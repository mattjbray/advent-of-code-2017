{-# LANGUAGE BinaryLiterals #-}
module Day15 where

import Data.Bits ((.&.))
import Data.Word (Word32)

generator :: Int -> (Int -> Bool) -> Int -> [Word32]
generator factor condition prev =
  let val = (prev * factor) `mod` 2147483647
      rest = generator factor condition val
  in if condition val then
    fromIntegral val : rest
  else
    rest

generatorA :: (Int -> Bool) -> Int -> [Word32]
generatorA = generator 16807

generatorB :: (Int -> Bool) -> Int -> [Word32]
generatorB = generator 48271

generators :: (Int, Int) -> [(Word32, Word32)]
generators (initA, initB) =
  zip (generatorA (const True) initA)
      (generatorB (const True) initB)

mask :: Word32
mask = 0b00000000000000001111111111111111

judgePair :: Word32 -> Word32 -> Bool
judgePair a b =
  a .&. mask == b .&. mask

judge :: Int -> [(Word32, Word32)] -> Int
judge n =
  length .
  filter (uncurry judgePair) .
  take n

part2Generators :: (Int, Int) -> [(Word32, Word32)]
part2Generators (initA, initB) =
  zip (generatorA (\i -> i `mod` 4 == 0) initA)
      (generatorB (\i -> i `mod` 8 == 0) initB)
