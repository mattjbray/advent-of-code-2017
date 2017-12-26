module Day14 where

import Data.Set (Set)
import qualified Data.Set as Set
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

type Pos = (Int, Int)

toPositions :: Grid -> Set Pos
toPositions =
  Set.fromList .
  concatMap
    (\(y, row) ->
        concatMap
          (\(x, col) ->
             if col == 1 then [(x, y)] else []
          )
          (zip [0..] row)
    ) .
  zip [0..] .
  getRows

neighbours :: Pos -> Set Pos
neighbours (x, y) =
  Set.fromList
    [ (x, y - 1)
    , (x - 1, y), (x + 1, y)
    , (x, y + 1)
    ]

regions :: Grid -> [Set Pos]
regions =
  buildNextRegion [] . toPositions
  where
    buildNextRegion :: [Set Pos] -> Set Pos -> [Set Pos]
    buildNextRegion regions remaining =
      case Set.minView remaining of
        Nothing -> regions
        Just (pos, remaining') ->
          let (region, remaining'') = findNeighbours remaining' pos
          in buildNextRegion (region : regions) remaining''

    findNeighbours :: Set Pos -> Pos -> (Set Pos, Set Pos)
    findNeighbours remaining pos =
      let ns = remaining `Set.intersection` (neighbours pos)
          remaining' = remaining `Set.difference` (neighbours pos)
      in
        foldl (\(region, remaining) n ->
                let (region', remaining') = findNeighbours remaining n
                in (region `Set.union` region', remaining')
                )
          (Set.singleton pos, remaining') .
        Set.toList $ ns
