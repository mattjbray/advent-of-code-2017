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
  garbageCount <-
    between (char '<') (char '>') $
      many $ choice
        [ char '!' >> anyChar >> return 0
        , satisfy (\c -> c /= '>') >> return 1
        ]
  return $ Garbage (sum garbageCount)

countGroups :: Group -> Int
countGroups (Group {gContents}) =
  1 +
  (sum .
   map
     (\contents ->
        case contents of
          CGroup g -> countGroups g
          CGarbage _ -> 0) $
   gContents)

score :: Group -> Int
score g = go 0 g
  where
    go :: Int -> Group -> Int
    go n (Group {gContents}) =
      n + 1 +
      (sum .
       map
         (\contents ->
            case contents of
              CGroup g' -> go (n + 1) g'
              CGarbage _ -> 0) $
       gContents)

countGarbage :: Group -> Int
countGarbage (Group {gContents}) =
  sum .
  map
    (\contents ->
       case contents of
         CGroup g -> countGarbage g
         CGarbage (Garbage i) -> i) $
  gContents
