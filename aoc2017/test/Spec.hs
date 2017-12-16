import Test.Tasty
import Test.Tasty.HUnit
import Day3

main :: IO ()
main = defaultMain $ testGroup "Advent of Code 2017"
  [ day3tests ]

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
