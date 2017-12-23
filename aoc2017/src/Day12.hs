module Day12 where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char

parseInput :: String -> Either (ParseError (Token String) ()) (IntMap (Set Int))
parseInput input =
  parse parser "" input
  where
    entry :: Parsec () String (Int, Set Int)
    entry = do
      k <- read <$> some digitChar
      string " <-> "
      vs <- map read <$>
        sepBy (some digitChar) (string ", ")
      return (k, S.fromList vs)
    parser :: Parsec () String (IntMap (Set Int))
    parser =
      M.fromList <$> sepEndBy entry eol

connectedPrograms :: Int -> IntMap (Set Int) -> Set Int
connectedPrograms n programs =
  go S.empty n
  where
    go :: Set Int -> Int -> Set Int
    go seen n =
      let seen' = S.insert n seen
          directs = S.difference (fromMaybe S.empty (M.lookup n programs)) seen'
          indirects = S.unions . map (go seen') . S.toList $ directs
      in S.unions [S.singleton n, directs, indirects]
