module Day4 where

import Data.List (sort)
import qualified Data.Set as Set

valid :: String -> Bool
valid input =
  let ws = words input
  in length ws == length (Set.fromList ws)

readPassphrases :: FilePath -> IO [String]
readPassphrases file = lines <$> readFile file


valid2 :: String -> Bool
valid2 input =
  let ws = words input
  in length ws == length (Set.fromList (map sort ws))
