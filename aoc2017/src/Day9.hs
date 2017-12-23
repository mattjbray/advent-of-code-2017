{-# LANGUAGE NamedFieldPuns #-}
module Day9
  ( Contents(..)
  , Garbage(..)
  , Group(..)
  , countGarbage
  , countGroups
  , garbage
  , group
  , parse
  , score
  )
where

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec () String

data Group = Group
  { gContents :: [Contents] }
  deriving (Eq, Show)

data Garbage = Garbage Int
  deriving (Eq, Show)

data Contents
  = CGroup Group
  | CGarbage Garbage
  deriving (Eq, Show)

group :: Parser Group
group = do
  gContents <-
    between (char '{') (char '}')
    (sepBy (CGroup <$> group <|> CGarbage <$> garbage) (char ','))
  return $ Group { gContents }

garbage :: Parser Garbage
garbage = do
  count <-
    between (char '<') (char '>') $
      many $ choice
        [ char '!' >> anyChar >> return 0
        , satisfy (\c -> c /= '>') >> return 1
        ]
  return $ Garbage (sum count)

countGroups :: Group -> Int
countGroups (Group {gContents}) =
  1 +
  (sum .
   map
     (\contents ->
        case contents of
          CGroup group -> countGroups group
          CGarbage _ -> 0) $
   gContents)

score :: Group -> Int
score group = go 0 group
  where
    go :: Int -> Group -> Int
    go n (Group {gContents}) =
      n + 1 +
      (sum .
       map
         (\contents ->
            case contents of
              CGroup group -> go (n + 1) group
              CGarbage _ -> 0) $
       gContents)

countGarbage :: Group -> Int
countGarbage (Group {gContents}) =
  sum .
  map
    (\contents ->
       case contents of
         CGroup group -> countGarbage group
         CGarbage (Garbage i) -> i) $
  gContents
