import Test.Tasty
import Test.Tasty.HUnit
import Day3

main :: IO ()
main = defaultMain tests

tests = testGroup "Day 3"
 [ testCase "Data from square 1 is carried 0 steps, since it's at the access port" $
   manhattanDistance 1 @?= 0
 , testCase "Data from square 12 is carried 3 steps, such as: down, left, left" $
   manhattanDistance 12 @?= 3
 , testCase "Data from square 23 is carried only 2 steps: up twice" $
   manhattanDistance 23 @?= 2
 , testCase "Data from square 1024 must be carried 31 steps" $
   manhattanDistance 1024 @?= 31
 , testCase "The solution is 438" $
   manhattanDistance 265149 @?= 438
 ]
