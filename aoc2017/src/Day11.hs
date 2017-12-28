{-# LANGUAGE NamedFieldPuns #-}
module Day11 where

import Text.Megaparsec
import Text.Megaparsec.Char

data Dir
  = N
  | NE
  | SE
  | S
  | SW
  | NW
  deriving (Show)

parseInput :: String -> Either (ParseError (Token String) ()) [Dir]
parseInput input =
  parse parser "" input
  where
    dir :: Parsec () String Dir
    dir =
      choice
        [ const NE <$> string "ne"
        , const NW <$> string "nw"
        , const N <$> string "n"
        , const SE <$> string "se"
        , const SW <$> string "sw"
        , const S <$> string "s"
        ]
    parser :: Parsec () String [Dir]
    parser = sepBy dir (char ',')


data CubeCoOrd = CubeCoOrd
  { x :: Int
  , y :: Int
  , z :: Int
  }
  deriving (Show)

step :: CubeCoOrd -> Dir -> CubeCoOrd
step CubeCoOrd { x, y, z} dir =
  case dir of
    N -> CubeCoOrd
      { x
      , y = y + 1
      , z = z - 1
      }
    NE -> CubeCoOrd
      { x = x + 1
      , y
      , z = z - 1
      }
    SE -> CubeCoOrd
      { x = x + 1
      , y = y - 1
      , z
      }
    S -> CubeCoOrd
      { x
      , y = y - 1
      , z = z + 1
      }
    SW -> CubeCoOrd
      { x = x - 1
      , y
      , z = z + 1
      }
    NW -> CubeCoOrd
      { x = x - 1
      , y = y + 1
      , z
      }

origin :: CubeCoOrd
origin = CubeCoOrd 0 0 0

distFromOrigin :: CubeCoOrd -> Int
distFromOrigin (CubeCoOrd {x, y, z}) =
  (abs x + abs y + abs z) `div` 2

distFromOriginAfterSteps :: [Dir] -> Int
distFromOriginAfterSteps =
  distFromOrigin . foldl step origin

maxDistFromOrigin :: [Dir] -> Int
maxDistFromOrigin =
  fst .
  foldl
    (\(maxDist, position) dir ->
       ( max maxDist (distFromOrigin position)
       , step position dir
       ))
  (0, origin)
