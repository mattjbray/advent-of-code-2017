{-# LANGUAGE OverloadedLists #-}

import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Vector                as V
import           Day3
import           Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Test.Tasty.Options

newtype SlowTests = SlowTests Bool

instance IsOption SlowTests where
  defaultValue = SlowTests False
  parseValue = fmap SlowTests . safeRead
  optionName = return "slow-tests"
  optionHelp = return "Run the slow tests"
  optionCLParser = flagCLParser Nothing (SlowTests True)

slowTestCase :: TestName -> Assertion -> TestTree
slowTestCase name assertion =
  askOption
    (\(SlowTests runSlowTests) ->
       (if runSlowTests
          then id
          else ignoreTest) $
       testCase name assertion)

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy SlowTests)] : defaultIngredients) $
  testGroup "Advent of Code 2017"
    [ day3tests
    , day4tests
    , day5tests
    , day6tests
    , day7tests
    , day8tests
    , day9tests
    , day10tests
    , day11tests
    ]

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
           , slowTestCase "The solution is 26395586" $ do
               instructions <- Day5.getInstructions
               steps <- Day5.steps Day5.part2 instructions
               steps @?= 26395586
           ]
       ]

day6tests =
  let example = [0, 2, 7, 0] in
  testGroup "Day 6"
    [ testGroup "Part 1"
        [ testCase "After one cycle" $
          iterate Day6.reallocateCycle example !! 1 @?= [2, 4, 1, 2]
        , testCase "After two cycles" $
          iterate Day6.reallocateCycle example !! 2 @?= [3, 1, 2, 3]
        , testCase "After three cycles" $
          iterate Day6.reallocateCycle example !! 3 @?= [0, 2, 3, 4]
        , testCase "After four cycles" $
          iterate Day6.reallocateCycle example !! 4 @?= [1, 3, 4, 1]
        , testCase "After five cycles" $
          iterate Day6.reallocateCycle example !! 5 @?= [2, 4, 1, 2]
        , testCase "It detects the loop" $
          Day6.detectLoop example @?= (5, [2, 4, 1, 2])
        , testCase "The solution is 5042" $
          fst (Day6.detectLoop Day6.input) @?= 5042
        ]
    , testGroup "Part 2"
        [ testCase "Example" $
          let (_, banks) = Day6.detectLoop example in
          fst (Day6.detectLoop banks) - 1 @?= 4
        , testCase "The solution is 1086" $
          let (_, banks) = Day6.detectLoop Day6.input in
          fst (Day6.detectLoop banks) - 1 @?= 1086
        ]
    ]

day7tests =
  testGroup "Day 7"
    [ testGroup "Part 1"
        [ testCase "tknk is at the bottom of the tower" $ do
          programs <- Day7.readPrograms "../resources/day_7_example.txt"
          Day7.bottomProgram . Day7.buildTree <$> programs @?= Just "tknk"
        , testCase "The solution is hlqnsbe" $ do
          programs <- Day7.readPrograms "../resources/day_7.txt"
          Day7.bottomProgram . Day7.buildTree <$> programs @?= Just "hlqnsbe"
        ]
    , testGroup "Part 2"
        [ testCase "the right weight would be 60" $ do
          programs <- Day7.readPrograms "../resources/day_7_example.txt"
          (Day7.sumWeights . Day7.buildTree <$> programs >>= Day7.wrongWeight) @?= Just 60
        , testCase "the solution is 1993" $ do
          programs <- Day7.readPrograms "../resources/day_7.txt"
          (Day7.sumWeights . Day7.buildTree <$> programs >>= Day7.wrongWeight) @?= Just 1993
        ]
    ]

day8tests =
  testGroup "Day 8"
    [ testGroup "Part 1"
        [ testCase "Because a starts at 0, it is not greater than 1, and so b is not modified" $
          Day8.run . take 1 <$> Day8.parseInstructions Day8.example @?= Right []
        , testCase "a is increased by 1 (to 1) because b is less than 5 (it is 0)" $
          Day8.run . take 2 <$> Day8.parseInstructions Day8.example @?= Right [("a", 1)]
        , testCase "c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1)" $
          Day8.run . take 3 <$> Day8.parseInstructions Day8.example @?= Right [("a", 1), ("c", 10)]
        , testCase "c is increased by -20 (to -10) because c is equal to 10" $
          Day8.run . take 4 <$> Day8.parseInstructions Day8.example @?= Right [("a", 1), ("c", -10)]
        , testCase "After this process, the largest value in any register is 1" $
          Day8.solve <$> Day8.parseInstructions Day8.example @?= Right 1
        , testCase "the solution is 3089" $ do
          is <- Day8.parseInstructions <$> readFile "../resources/day_8.txt"
          Day8.solve <$> is @?= Right 3089
        ]
    , testGroup "Part 2"
        [ testCase "the highest value ever held was 10" $
          Day8.runMax <$> Day8.parseInstructions Day8.example @?= Right 10
        , testCase "the solution is 5391" $ do
          is <- Day8.parseInstructions <$> readFile "../resources/day_8.txt"
          Day8.runMax <$> is @?= Right 5391
        ]
    ]

day9tests =
  let puzzleInput =
        Day9.parse Day9.group "day_9.txt" <$> readFile "../resources/day_9.txt"
  in
  testGroup "Day 9"
    [ testGroup "Part 1"
      [ testGroup "Here are some self-contained pieces of garbage"
        [ testCase "empty garbage" $
          Day9.parse Day9.garbage "" "<>" @?= Right (Day9.Garbage 0)
        , testCase "garbage containing random characters" $
          Day9.parse Day9.garbage "" "<random characters>" @?= Right (Day9.Garbage 17)
        , testCase "because the extra < are ignored" $
          Day9.parse Day9.garbage "" "<<<<>" @?= Right (Day9.Garbage 3)
        , testCase "because the first > is canceled" $
          Day9.parse Day9.garbage "" "<{!>}>" @?= Right (Day9.Garbage 2)
        , testCase "because the second ! is canceled, allowing the > to terminate the garbage" $
          Day9.parse Day9.garbage "" "<!!>" @?= Right (Day9.Garbage 0)
        , testCase "because the second ! and the first > are canceled" $
          Day9.parse Day9.garbage "" "<!!!>>" @?= Right (Day9.Garbage 0)
        , testCase "which ends at the first >" $
          Day9.parse Day9.garbage "" "<{o\"i!a,<{i<a>" @?= Right (Day9.Garbage 10)
        ]
      , testGroup "Here are some examples of whole streams and the number of groups they contain"
        [ testCase "1 group" $
          Day9.countGroups <$> Day9.parse Day9.group "" "{}" @?= Right 1
        , testCase "3 groups" $
          Day9.countGroups <$> Day9.parse Day9.group "" "{{{}}}" @?= Right 3
        , testCase "also 3 groups" $
          Day9.countGroups <$> Day9.parse Day9.group "" "{{},{}}" @?= Right 3
        , testCase "6 groups" $
          Day9.countGroups <$> Day9.parse Day9.group "" "{{{},{},{{}}}}" @?= Right 6
        , testCase "1 group (which itself contains garbage)" $
          Day9.countGroups <$> Day9.parse Day9.group "" "{<{},{},{{}}>}" @?= Right 1
        , testCase "1 group" $
          Day9.countGroups <$> Day9.parse Day9.group "" "{<a>,<a>,<a>,<a>}" @?= Right 1
        , testCase "5 groups" $
          Day9.countGroups <$> Day9.parse Day9.group "" "{{<a>},{<a>},{<a>},{<a>}}" @?= Right 5
        , testCase "2 groups (since all but the last > are canceled)" $
          Day9.countGroups <$> Day9.parse Day9.group "" "{{<!>},{<!>},{<!>},{<a>}}" @?= Right 2
        ]
      , testGroup "Each group is assigned a score which is one more than the score of the group that immediately contains it"
        [ testCase "score of 1" $
          Day9.score <$> Day9.parse Day9.group "" "{}" @?= Right 1
        , testCase "score of 1 + 2 + 3 = 6" $
          Day9.score <$> Day9.parse Day9.group "" "{{{}}}" @?= Right 6
        , testCase "score of 1 + 2 + 2 = 5" $
          Day9.score <$> Day9.parse Day9.group "" "{{},{}}" @?= Right 5
        , testCase "score of 1 + 2 + 3 + 3 + 3 + 4 = 16" $
          Day9.score <$> Day9.parse Day9.group "" "{{{},{},{{}}}}" @?= Right 16
        , testCase "score of 1" $
          Day9.score <$> Day9.parse Day9.group "" "{<a>,<a>,<a>,<a>}" @?= Right 1
        , testCase "score of 1 + 2 + 2 + 2 + 2 = 9" $
          Day9.score <$> Day9.parse Day9.group "" "{{<ab>},{<ab>},{<ab>},{<ab>}}" @?= Right 9
        , testCase "score of 1 + 2 + 2 + 2 + 2 = 9" $
          Day9.score <$> Day9.parse Day9.group "" "{{<!!>},{<!!>},{<!!>},{<!!>}}" @?= Right 9
        , testCase "score of 1 + 2 = 3" $
          Day9.score <$> Day9.parse Day9.group "" "{{<a!>},{<a!>},{<a!>},{<ab>}}" @?= Right 3
        ]
      , testCase "the solution is 17537" $ do
          group <- puzzleInput
          Day9.score <$> group @?= Right 17537
      ]
    , testGroup "Part 2"
      [ testCase "the solution is 7539" $ do
          group <- puzzleInput
          Day9.countGarbage <$> group @?= Right 7539
      ]
    ]

day10tests =
  testGroup "Day 10"
    [ let listSize = 5
          exampleLengths = [3, 4, 1, 5]
      in testGroup "Part 1"
         [ testCase "after 1 step" $
           Day10.processLengths listSize (take 1 exampleLengths) @?= [2, 1, 0, 3, 4]
         , testCase "after 2 steps" $
           Day10.processLengths listSize (take 2 exampleLengths) @?= [4, 3, 0, 1, 2]
         , testCase "after 3 steps" $
           Day10.processLengths listSize (take 3 exampleLengths) @?= [4, 3, 0, 1, 2]
         , testCase "after 4 steps" $
           Day10.processLengths listSize (take 4 exampleLengths) @?= [3, 4, 2, 1, 0]
         , testCase "check the process" $
           Day10.solve listSize exampleLengths @?= 12
         , testCase "the solution is 11375" $ do
           lengths <- Day10.parseInput <$> readFile "../resources/day_10.txt"
           Day10.solve 256 <$> lengths @?= Right 11375
         ]
    , let exampleLengths = [3, 4, 1, 5] ++ Day10.lengthsSuffix
      in testGroup "Part 2"
      [ testCase "if you are given 1,2,3, your final sequence of lengths should be 49,44,50,44,51,17,31,73,47,23" $
        Day10.inputToBytes "1,2,3" @?= [49,44,50,44,51,17,31,73,47,23]
      , testGroup "Here are some example hashes"
        [ testCase "The empty string becomes a2582a3a0e66e6e86e3812dcb672a272" $
          Day10.knotHash "" @?= "a2582a3a0e66e6e86e3812dcb672a272"
        , testCase "AoC 2017 becomes 33efeb34ea91902bb2f59c9920caa6cd" $
          Day10.knotHash "AoC 2017" @?= "33efeb34ea91902bb2f59c9920caa6cd"
        , testCase "1,2,3 becomes 3efbe78a8d82f29979031a4aa0b16a9d" $
          Day10.knotHash "1,2,3" @?= "3efbe78a8d82f29979031a4aa0b16a9d"
        , testCase "1,2,4 becomes 63960835bcdc130f0b66d7ff4f6a5a8e" $
          Day10.knotHash "1,2,4" @?= "63960835bcdc130f0b66d7ff4f6a5a8e"
        ]
      , testCase "the solution is e0387e2ad112b7c2ef344e44885fe4d8" $ do
        hash <- Day10.knotHash <$> readFile "../resources/day_10.txt"
        hash @?= "e0387e2ad112b7c2ef344e44885fe4d8"
      ]
    ]

day11tests =
  testGroup "Day 11"
    [ testGroup "Part 1"
      [ testCase "ne,ne,ne is 3 steps away." $
        Day11.distFromOriginAfterSteps <$> Day11.parseInput "ne,ne,ne" @?= Right 3
      , testCase "ne,ne,sw,sw is 0 steps away (back where you started)." $
        Day11.distFromOriginAfterSteps <$> Day11.parseInput "ne,ne,sw,sw" @?= Right 0
      , testCase "ne,ne,s,s is 2 steps away (se,se)." $
        Day11.distFromOriginAfterSteps <$> Day11.parseInput "ne,ne,s,s" @?= Right 2
      , testCase "se,sw,se,sw,sw is 3 steps away (s,s,sw)."  $
        Day11.distFromOriginAfterSteps <$> Day11.parseInput "se,sw,se,sw,sw" @?= Right 3
      , testCase "the solution is 784" $ do
        steps <- Day11.parseInput <$> readFile "../resources/day_11.txt"
        Day11.distFromOriginAfterSteps <$> steps @?= Right 784
      ]
    ]
