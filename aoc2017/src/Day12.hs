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
      _ <- string " <-> "
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
    go seen m =
      let seen' = S.insert m seen
          directs = S.difference (fromMaybe S.empty (M.lookup m programs)) seen'
          indirects = S.unions . map (go seen') . S.toList $ directs
      in S.unions [S.singleton m, directs, indirects]

groups :: IntMap (Set Int) -> Set (Set Int)
groups programs =
  fst .
  foldl (\(result, seen) n ->
           if S.member n seen then
             (result, seen)
           else
             let group = connectedPrograms n programs
             in (S.insert group result, S.union group seen )
           )
  (S.empty, S.empty) $
  (M.keys programs)
