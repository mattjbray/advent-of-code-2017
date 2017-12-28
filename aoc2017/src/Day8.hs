{-# LANGUAGE NamedFieldPuns #-}
module Day8 where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Megaparsec

example :: String
example = unlines
  [ "b inc 5 if a > 1"
  , "a inc 1 if b < 5"
  , "c dec -10 if a >= 1"
  , "c inc -20 if c == 10"
  ]

data Condition = Condition
  { cRegister :: String
  , cPredicate :: Int -> Bool
  }

data Instruction = Instruction
  { iRegister :: String
  , iOperation :: Int -> Int
  , iCondition :: Condition
  }

type Parser = Parsec () String

int :: Parser Int
int =
  read <$> do
    sign <- option "" (string "-")
    digits <- some numberChar
    return $ sign ++ digits

instruction :: Parser Instruction
instruction = do
  iRegister <- some letterChar
  _ <- spaceChar
  iOperation <- operation
  _ <- string " if "
  iCondition <- condition
  _ <- eol
  return $ Instruction { iRegister, iOperation, iCondition }

operation :: Parser (Int -> Int)
operation = do
  op <- choice
    [ const (+) <$> string "inc"
    , const (-) <$> string "dec"
    ]
  _ <- spaceChar
  val <- int
  return $ \i -> i `op` val

condition :: Parser Condition
condition = do
  cRegister <- some letterChar
  cPredicate <- do
    op <- choice
      [ const (>)  <$> string " > "
      , const (>=) <$> string " >= "
      , const (<)  <$> string " < "
      , const (<=) <$> string " <= "
      , const (==) <$> string " == "
      , const (/=) <$> string " != "
      ]
    val <- int
    return $ \x -> x `op` val
  return $ Condition { cRegister, cPredicate }

parseInstructions :: String -> Either (ParseError (Token String) ()) [Instruction]
parseInstructions = parse (many instruction) ""

interpret :: Map String Int -> Instruction -> Map String Int
interpret env inst =
  let cond = iCondition inst
      cVal = fromMaybe 0 $ M.lookup (cRegister cond) env
  in if (cPredicate cond) cVal then
       let register = iRegister inst
           val = fromMaybe 0 $ M.lookup register env
       in M.insert register ((iOperation inst) val) env
     else
       env

run :: [Instruction] -> Map String Int
run = foldl interpret M.empty

solve :: [Instruction] -> Int
solve = maximum . M.elems . run

runMax :: [Instruction] -> Int
runMax =
  snd .
  foldl
    (\(env, m) i ->
       let env' = interpret env i
           m' = foldl max 0 . M.elems $ env'
       in (env', max m m'))
    (M.empty, 0)
