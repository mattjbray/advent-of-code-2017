module Day5 where

import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV

jump :: (Int -> Int) -> (Int, IOVector Int) -> IO (Int, IOVector Int)
jump f (i, v) = do
  inst <- MV.read v i
  MV.modify v f i
  return (i + inst, v)

part1 :: Int -> Int
part1 = (+) 1

part2 :: Int -> Int
part2 inst =
  if inst >= 3
    then inst - 1
    else inst + 1

iterateWhileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m [a]
iterateWhileM cond action x =
  if cond x
    then do
      next <- action x
      xs <- iterateWhileM cond action next
      return $ x : xs
    else return []

steps :: (Int -> Int) -> Vector Int -> IO Int
steps f v =
  let len = V.length v
  in do mv <- V.thaw v
        js <- iterateWhileM (\(i, _) -> i < len) (jump f) (0, mv)
        return (length js)

getInstructions :: IO (Vector Int)
getInstructions =
  V.fromList . map read . lines <$> readFile "../resources/day_5.txt"
