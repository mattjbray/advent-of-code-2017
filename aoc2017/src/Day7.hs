{-# LANGUAGE NamedFieldPuns #-}
module Day7 where

import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Tree
import Data.List (partition, elem)

type Parser = Parsec () String

data Program = Program
  { pName :: String
  , pWeight :: Int
  , pSubPrograms :: [String]
  }
  deriving (Show)

program :: Parser Program
program = do
  pName <- some letterChar
  spaceChar
  pWeight <-
    between (char '(') (char ')')
     (read <$> some numberChar)
  pSubPrograms <- subPrograms
  eol
  return $ Program {pName, pWeight, pSubPrograms}

subPrograms :: Parser [String]
subPrograms =
    option [] $ do
      string " -> "
      some letterChar `sepBy` string ", "

readPrograms :: FilePath -> IO (Maybe [Program])
readPrograms file =
  parseMaybe (many program) <$> readFile file

type ProgramTree = Tree (String, Int)

buildLevel :: ([Program], [ProgramTree]) -> ([Program], [ProgramTree])
buildLevel (programs, trees) =
  foldl
  (\(programs, nodes) program ->
           let (childTrees, rest) =
                 partition ((flip elem) (pSubPrograms program) . fst . rootLabel) nodes
           in
           if length childTrees == length (pSubPrograms program) then
             (programs, (Node (pName program, pWeight program) childTrees) : rest)
           else
             (program : programs, nodes)
     )
  ([], trees)
  programs

buildTree :: [Program] -> ProgramTree
buildTree programs =
  head . snd . head . dropWhile (not . null . fst) . iterate buildLevel $ (programs, [])

bottomProgram :: ProgramTree -> String
bottomProgram = fst . rootLabel
