{-# LANGUAGE NamedFieldPuns #-}
module Day18 where

import Data.Maybe (fromMaybe, maybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

data Val
  = Register Char
  | Number Int
  deriving (Show, Eq)

data Op = Set | Add | Mul | Mod
  deriving (Show, Eq)

data Instruction
  = Snd Val
  | Op Op Char Val
  | Rcv Char
  | Jgz Val Val
  deriving (Show, Eq)

data State
  = Running
  | Blocked
  | Halted
  deriving (Show, Eq)

data Program = Program
  { inst :: Int
  , lastFreqPlayed :: Int
  , numSnds :: Int
  , state :: State
  , registers :: Map Char Int
  , rcvQ :: Seq Int
  , sndQ :: Seq Int
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
    [ string "snd " >> Snd <$> valP
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
          , numSnds = 0
          , state = Running
          , rcvQ = Seq.empty
          , sndQ = Seq.empty
          }

type SndHandler = Program -> Val -> Program
type RcvHandler = Program -> Char -> Program

part1OnSnd :: SndHandler
part1OnSnd program@(Program {registers, inst}) val =
  program
    { lastFreqPlayed = getValue registers val
    , inst = inst + 1
    }

part1OnRcv :: RcvHandler
part1OnRcv program@(Program {registers, inst}) register =
  let value = fromMaybe 0 (Map.lookup register registers)
  in program
      { state = if value == 0 then Running else Halted
      , inst = inst + 1
      }

step :: SndHandler -> RcvHandler -> Seq Instruction -> Program -> Program
step onSnd onRcv instructions program@(Program {registers, inst})=
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
      onSnd program register

    Just (Rcv register) ->
      onRcv program register

    Just (Jgz register offset) ->
      let value = getValue registers register
      in program
         { inst = inst + if value > 0 then (getValue registers offset) else 1
         }

    Nothing ->
      program { state = Halted }

getValue :: Map Char Int -> Val -> Int
getValue _ (Number n) = n
getValue registers (Register c) =
  case Map.lookup c registers of
    Nothing -> 0
    Just n -> n

run :: SndHandler -> RcvHandler -> Seq Instruction -> Program -> Program
run onSnd onRcv instructions =
  head .
  dropWhile ((== Running) . state) .
  iterate (step onSnd onRcv instructions)

runPart1 :: Seq Instruction -> Program -> Program
runPart1 = run part1OnSnd part1OnRcv


-- Part 2

part2OnSnd :: SndHandler
part2OnSnd program@(Program {registers, inst, sndQ, numSnds}) val =
  program
    { inst = inst + 1
    , sndQ = sndQ Seq.|> getValue registers val
    , numSnds = numSnds + 1
    }

part2OnRcv :: RcvHandler
part2OnRcv program@(Program {registers, inst, rcvQ}) register =
  case rcvQ of
    val Seq.:<| rcvQ' ->
      program
          { state = Running
          , inst = inst + 1
          , rcvQ = rcvQ'
          , registers = Map.insert register val registers
          }
    Seq.Empty ->
      program { state = Blocked }

runPart2 :: Seq Instruction -> Program -> Program
runPart2 = run part2OnSnd part2OnRcv

data Programs = Programs
  { program0 :: Program
  , program1 :: Program
  }
  deriving (Show, Eq)

initPrograms :: Programs
initPrograms =
  Programs
    { program0 = initProgram { registers = Map.singleton 'p' 0 }
    , program1 = initProgram { registers = Map.singleton 'p' 1 }
    }

step2 :: Seq Instruction -> Programs -> Programs
step2 instructions (Programs { program0 = p0, program1 = p1 })  =
  let p0' = p0 { sndQ = Seq.empty, rcvQ = rcvQ p0 Seq.>< sndQ p1, state = Running }
      p1' = p1 { sndQ = Seq.empty, rcvQ = rcvQ p1 Seq.>< sndQ p0, state = Running }
      p0'' = runPart2 instructions p0'
      p1'' = runPart2 instructions p1'
  in Programs { program0 = p0'', program1 = p1'' }

run2 :: Seq Instruction -> Programs -> Programs
run2 instructions programs@(Programs { program0 = p0, program1 = p1 }) =
  case (state p0, sndQ p0, state p1, sndQ p1) of
      (Halted, _, Halted, _) -> programs
      (Blocked, Seq.Empty, Blocked, Seq.Empty) -> programs
      _ -> run2 instructions (step2 instructions programs)
