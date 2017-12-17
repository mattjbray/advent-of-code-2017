{-# LANGUAGE OverloadedLists #-}

import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit
import Day3
import Day4
import qualified Day5

main :: IO ()
main = defaultMain $ testGroup "Advent of Code 2017"
  [ day3tests, day4tests, day5tests ]

day3tests =
  testGroup
    "Day 3"
    [ testGroup
        "Part 1"
        [ testCase
            "Data from square 1 is carried 0 steps, since it's at the access port" $
          manhattanDistance 1 @?= 0
        , testCase
            "Data from square 12 is carried 3 steps, such as: down, left, left" $
          manhattanDistance 12 @?= 3
        , testCase "Data from square 23 is carried only 2 steps: up twice" $
          manhattanDistance 23 @?= 2
        , testCase "Data from square 1024 must be carried 31 steps" $
          manhattanDistance 1024 @?= 31
        , testCase "The solution is 438" $ manhattanDistance 265149 @?= 438
        ]
    , testGroup
        "Part 2"
        [ testCase "Square 1 starts with the value 1." $ squareValues !! 0 @?= 1
        , testCase
            "Square 2 has only one adjacent filled square (with value 1), so it also stores 1." $
          squareValues !! 1 @?= 1
        , testCase
            "Square 3 has both of the above squares as neighbors and stores the sum of their values, 2." $
          squareValues !! 2 @?= 2
        , testCase
            "Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4." $
          squareValues !! 3 @?= 4
        , testCase
            "Square 5 only has the first and fourth squares as neighbors, so it gets the value 5." $
          squareValues !! 4 @?= 5
        , testCase "The solution is 266330" $
          (head . dropWhile (< 265149) $ squareValues) @?= 266330
        ]
    ]

day4tests =
  testGroup
    "Day 4"
    [ testGroup
        "Part 1"
        [ testCase "aa bb cc dd ee is valid" $
          valid "aa bb cc dd ee" @? "Is not valid"
        , testCase
            "aa bb cc dd aa is not valid - the word aa appears more than once" $
          not (valid "aa bb cc dd aa") @? "Is valid"
        , testCase
            "aa bb cc dd aaa is valid - aa and aaa count as different words" $
          valid "aa bb cc dd aaa" @? "Is not valid"
        , testCase "The solution is 337" $ do
            phrases <- readPassphrases "../resources/day_4.txt"
            (length . filter valid) phrases @?= 337
        ]
    , testGroup
        "Part 2"
        [ testCase "abcde fghij is a valid passphrase" $
          valid2 "abcde fghij" @? "Is not valid"
        , testCase
            "abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word" $
          not (valid2 "abcde xyz ecdab") @? "Is valid"
        , testCase
            "a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word" $
          valid2 "a ab abc abd abf abj" @? "Is not valid"
        , testCase "iiii oiii ooii oooi oooo is valid" $
          valid2 "iiii oiii ooii oooi oooo" @? "Is not valid"
        , testCase
            "oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word" $
          not (valid2 "oiii ioii iioi iiio") @? "Is valid"
        , testCase "The solution is 231" $ do
            phrases <- readPassphrases "../resources/day_4.txt"
            (length . filter valid2) phrases @?= 231
        ]
    ]


day5tests =
  let example = [0, 3, 0, 1, -3]
  in testGroup
       "Day 5"
       [ testGroup
           "Part 1"
           [ testCase "The example takes 5 steps" $ do
               steps <- Day5.steps Day5.part1 example
               steps @?= 5
           , testCase "The solution is 373160" $ do
               instructions <- Day5.getInstructions
               steps <- Day5.steps Day5.part1 instructions
               steps @?= 373160
           ]
       , testGroup
           "Part 2"
           [ testCase "The example takes 10 steps" $ do
               steps <- Day5.steps Day5.part2 example
               steps @?= 10
           , testCase "The solution is 26395586" $ do
               instructions <- Day5.getInstructions
               steps <- Day5.steps Day5.part2 instructions
               steps @?= 26395586
           ]
       ]
