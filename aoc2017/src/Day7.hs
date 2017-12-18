{-# LANGUAGE NamedFieldPuns #-}
module Day7 where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Tree
import Data.List (partition, elem, find)
import qualified Data.MultiSet as MS

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

sumWeights :: ProgramTree -> Tree (String, Int, Int)
sumWeights (Node (name, weight) children) =
  let children' = sumWeights <$> children
      childWeight = sum ((\(_,_,w) -> w) . rootLabel <$> children')
  in Node (name, weight, weight + childWeight) children'

wrongWeight :: Tree (String, Int, Int) -> Maybe Int
wrongWeight (Node _ children) =
  case asum (wrongWeight <$> children) of
    Just x -> Just x
    Nothing ->
      let childWeights =
            MS.fromList ((\(_, _, w) -> w) . rootLabel <$> children)
      in if MS.distinctSize childWeights > 1 then
        case leastAndMostCommonElems childWeights of
          Just (wrongWeight, correctWeight) -> do
            Node (_, weight, _) _ <- find (\(Node (_, _, w) _) -> w == wrongWeight) children
            Just $ weight + correctWeight - wrongWeight
          _ -> Nothing
      else Nothing

{-| Get the least and most frequently occurring elements of a MultiSet. -}
leastAndMostCommonElems :: MS.MultiSet a -> Maybe (a, a)
leastAndMostCommonElems =
  fmap (\(least,_,most,_) -> (least, most)) .
  MS.foldOccur
    (\elem occur acc ->
       case acc of
         Nothing -> Just (elem, occur, elem, occur)
         Just (leastElem, leastOccur, mostElem, mostOccur) ->
           if occur < leastOccur then
             Just (elem, occur, mostElem, mostOccur)
           else if occur > mostOccur then
             Just (leastElem, leastOccur, elem, occur)
           else
             acc
             )
    Nothing
