module Day16 where

import Data.List (splitAt, foldl')
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char


type Programs = Vector Char

line :: Programs
line = V.fromList ['a'..'p']

data Move
  = Spin Int
  | Exchange Int Int
  | Partner Char Char

type Parser = Parsec () String

spin :: Parser Move
spin = do
  char 's'
  i <- read <$> some digitChar
  return $ Spin i

exchange :: Parser Move
exchange = do
  char 'x'
  a <- read <$> some digitChar
  char '/'
  b <- read <$> some digitChar
  return $ Exchange a b

partner :: Parser Move
partner = do
  char 'p'
  a <- letterChar
  char '/'
  b <- letterChar
  return $ Partner a b

move :: Parser Move
move =
  choice
    [ spin
    , exchange
    , partner
    ]

parseMoves :: String -> Either (ParseError (Token String) ()) [Move]
parseMoves = parse (sepBy move (char ',')) ""

danceMove :: Programs -> Move -> Programs
danceMove ps (Spin x) =
  let (front, back) = V.splitAt (length ps - x) ps
  in back V.++ front
danceMove ps (Exchange indexA indexB) =
  let programA = ps V.! indexA
      programB = ps V.! indexB
  in
    ps V.// [(indexA, programB), (indexB, programA)]
danceMove ps (Partner programA programB) =
  fromMaybe ps $ do
    indexA <- V.elemIndex programA ps
    indexB <- V.elemIndex programB ps
    return $ ps V.// [(indexA, programB), (indexB, programA)]

dance :: Programs -> [Move] -> Programs
dance = foldl' danceMove
