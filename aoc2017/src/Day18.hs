{-# LANGUAGE NamedFieldPuns #-}
module Day18 where

import Data.Maybe (fromMaybe, maybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Megaparsec
import Text.Megaparsec.Char

data Val
  = Register Char
  | Number Int
  deriving (Show, Eq)

data Op = Set | Add | Mul | Mod
  deriving (Show, Eq)

data Instruction
  = Snd Char
  | Op Op Char Val
  | Rcv Char
  | Jgz Val Val
  deriving (Show, Eq)

data Program = Program
  { inst :: Int
  , lastFreqPlayed :: Int
  , halted :: Bool
  , registers :: Map Char Int
  }
  deriving (Show, Eq)

type Parser = Parsec () String

valP :: Parser Val
valP =
  choice
    [ Register <$> lowerChar
    , Number . read <$> some (char '-' <|> digitChar)
    ]

instructionP :: Parser Instruction
instructionP =
  choice
    [ string "snd " >> Snd <$> lowerChar
    , string "set " >> Op Set <$> lowerChar <*> (char ' ' >> valP)
    , string "add " >> Op Add <$> lowerChar <*> (char ' ' >> valP)
    , string "mul " >> Op Mul <$> lowerChar <*> (char ' ' >> valP)
    , string "mod " >> Op Mod <$> lowerChar <*> (char ' ' >> valP)
    , string "rcv " >> Rcv <$> lowerChar
    , string "jgz " >> Jgz <$> valP <*> (char ' ' >> valP)
    ]

tabletP :: Parser (Seq Instruction)
tabletP =
  Seq.fromList <$> sepBy instructionP eol

parseInput :: String -> Either (ParseError Char ()) (Seq Instruction)
parseInput = parse tabletP ""

initProgram :: Program
initProgram =
  Program { registers = Map.empty
          , inst = 0
          , lastFreqPlayed = 0
          , halted = False
          }

step :: Seq Instruction -> Program -> Program
step instructions program@(Program {registers, inst})=
  case instructions Seq.!? inst of
    Just (Op op register val) ->
      let value = getValue registers val
          fn :: Maybe Int -> Maybe Int
          fn = case op of
                 Set -> const (Just value)
                 Add -> Just . maybe value (+ value)
                 Mul -> fmap (* value)
                 Mod -> fmap (`mod` value)
      in
      program
        { registers = Map.alter fn register registers
        , inst = inst + 1
        }

    Just (Snd register) ->
      program
        { lastFreqPlayed = fromMaybe 0 (Map.lookup register registers)
        , inst = inst + 1
        }

    Just (Rcv register) ->
      let value = fromMaybe 0 (Map.lookup register registers)
      in program
         { halted = value /= 0
         , inst = inst + 1
         }

    Just (Jgz register offset) ->
      let value = getValue registers register
      in program
         { inst = inst + if value > 0 then (getValue registers offset) else 1
         }

    _ ->
      program { halted = True }

getValue :: Map Char Int -> Val -> Int
getValue _ (Number n) = n
getValue registers (Register c) =
  case Map.lookup c registers of
    Nothing -> 0
    Just n -> n

run :: Seq Instruction -> Program -> Program
run instructions =
  head .
  dropWhile (not . halted) .
  iterate (step instructions)
