module Day16 where

import Debug.Trace
import Data.List (splitAt, foldl')
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
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

danceSpin :: Programs -> Int -> Programs
danceSpin ps x =
  let (front, back) = V.splitAt (length ps - x) ps
  in back V.++ front

danceExchange :: Programs -> Int -> Int -> Programs
danceExchange ps indexA indexB =
  V.modify
   (\ps -> MV.unsafeSwap ps indexA indexB)
  ps

dancePartner :: Programs -> Char -> Char -> Programs
dancePartner ps programA programB =
  let (indexA, indexB) =
        fromMaybe (0,0) $ do
          indexA <- V.elemIndex programA ps
          indexB <- V.elemIndex programB ps
          return $ (indexA, indexB)
  in V.modify (\ps -> MV.unsafeSwap ps indexA indexB) ps

danceMove :: Programs -> Move -> Programs
danceMove ps (Spin x) = danceSpin ps x
danceMove ps (Exchange indexA indexB) = danceExchange ps indexA indexB
danceMove ps (Partner programA programB) = dancePartner ps programA programB

dance :: Programs -> [Move] -> Programs
dance = foldl' danceMove

dances :: Int -> Programs -> [Move] -> Programs
dances n ps moves =
  foldl' (\ps i ->
            (if i `mod` 1000 == 0 then traceShow i else id)
            dance ps moves) ps [0..n-1]
