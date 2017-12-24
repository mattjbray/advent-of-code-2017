module Day14 where

import qualified Day10

newtype Grid = Grid
  { getRows :: [[Int]] }

instance Show Grid where
  show =
    unlines .
    map (map (\b -> if b == 1 then '#' else '.')) .
    getRows

grid :: String -> Grid
grid key =
  Grid $
  map
    (concatMap hexToBits . Day10.knotHash . ((key ++ "-") ++) . show)
    [0 .. 127]

hexToBits :: Char -> [Int]
hexToBits '0' = [0, 0, 0, 0]
hexToBits '1' = [0, 0, 0, 1]
hexToBits '2' = [0, 0, 1, 0]
hexToBits '3' = [0, 0, 1, 1]
hexToBits '4' = [0, 1, 0, 0]
hexToBits '5' = [0, 1, 0, 1]
hexToBits '6' = [0, 1, 1, 0]
hexToBits '7' = [0, 1, 1, 1]
hexToBits '8' = [1, 0, 0, 0]
hexToBits '9' = [1, 0, 0, 1]
hexToBits 'a' = [1, 0, 1, 0]
hexToBits 'b' = [1, 0, 1, 1]
hexToBits 'c' = [1, 1, 0, 0]
hexToBits 'd' = [1, 1, 0, 1]
hexToBits 'e' = [1, 1, 1, 0]
hexToBits 'f' = [1, 1, 1, 1]

countUsed :: Grid -> Int
countUsed =
  sum . map sum . getRows
