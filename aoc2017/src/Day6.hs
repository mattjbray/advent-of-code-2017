{-# LANGUAGE OverloadedLists #-}
module Day6 where

import qualified Data.Set    as Set
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

redistribute :: Int -> Int -> Vector Int -> Vector Int
redistribute i remaining banks =
  if i >= V.length banks then
    redistribute 0 remaining banks
  else if remaining == 0 then
      banks
  else
    let bankVal = banks ! i in
    redistribute (i + 1) (remaining - 1) ( banks // [(i, bankVal + 1)])


reallocateCycle :: Vector Int -> Vector Int
reallocateCycle banks =
  let i = V.maxIndex banks
      maxVal = banks ! i
      banks' = banks // [(i, 0)]
  in
  redistribute (i + 1) maxVal banks'

detectLoop :: Vector Int -> (Int, Vector Int)
detectLoop banks =
  go Set.empty 1 banks
  where
    go seen i bs =
      let bs' = reallocateCycle bs
      in
        if Set.member bs' seen then
          (i, bs')
        else
          go (Set.insert bs' seen) (i + 1) bs'


input :: Vector Int
input = [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]
