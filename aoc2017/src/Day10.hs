{-# LANGUAGE NamedFieldPuns #-}
module Day10 where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (intercalate, mapAccumL)
import Data.List.Split (chunksOf)
import Text.Printf (printf)
import Data.Bits (xor)

data Acc = Acc
  { xs :: Vector Int
  , position :: Int
  , skipSize :: Int
  }

instance Show Acc where
  show (Acc {xs, position, skipSize}) =
    "(" ++
    concatMap
     (\(i, x) ->
        if i == position then
         "[" ++ show x ++ "]"
        else
         " " ++ show x ++ " "
     )
    (zip [0..] (V.toList xs))
    ++ ")"

initAcc :: Int -> Acc
initAcc listSize = Acc
  { xs = V.fromList [0..(listSize - 1)]
  , position = 0
  , skipSize = 0
  }

example :: Vector Int
example =
  V.fromList [0..4]

processLength :: Acc -> Int -> Acc
processLength (Acc { xs, position, skipSize }) length =
  let len = V.length xs
      end = (position + length) `mod` len
      xs' =
        if length == 0 then
          xs
        else if position < end then
          V.concat
            [ V.take position xs
            , V.reverse (V.slice position length xs)
            , V.drop end xs
            ]
        else
          let subList = V.drop position xs V.++ V.take end xs
              subListReversed = V.reverse subList
              (part1, part2) = V.splitAt (len - position) subListReversed
          in
          V.concat
            [ part2
            , V.slice end (position - end) xs
            , part1
            ]
  in
    Acc { xs = xs'
        , position = (position + length + skipSize) `mod` len
        , skipSize = skipSize + 1
        }

processLengths :: Int -> [Int] -> Vector Int
processLengths listSize =
  xs . foldl processLength (initAcc listSize)

debug listSize lengths =
  let (final, hist) =
        mapAccumL (\acc length ->
                     let acc' = processLength acc length in
                       (acc', acc)
                  )
        (initAcc listSize)
        lengths
  in
    intercalate "\n" . map show $ hist ++ [final]

solve :: Int -> [Int] -> Int
solve listSize = product . V.take 2 . processLengths listSize

parseInput :: String -> Either (ParseError (Token String) ()) [Int]
parseInput input =
  parse parser "" input
  where
    parser :: Parsec () String [Int]
    parser = sepBy (read <$> some digitChar) (char ',')

-- Part 2

lengthsSuffix = [17, 31, 73, 47, 23]

inputToBytes :: String -> [Int]
inputToBytes = (++ lengthsSuffix) . map fromEnum

denseHash :: [Int] -> [Int]
denseHash sparseHash =
  map (foldl1 xor) . chunksOf 16  $ sparseHash

toHex :: Int -> String
toHex = printf "%02x"

knotHash :: String -> String
knotHash =
  concatMap toHex .
  denseHash .
  V.toList . processLengths 256 .
  concat . take 64 . repeat .
  inputToBytes
