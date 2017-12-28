module Day16 where

import Control.Monad (replicateM_)
import Control.Monad.ST (ST)
import Debug.Trace
import Data.List (splitAt, foldl')
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as MV
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

danceSpin :: STVector s Char -> Int -> ST s ()
danceSpin ps x =
  let offset = MV.length ps - x
  in do
    copy <- MV.clone ps
    forM_ [0..x - 1] $ \i -> do
      program <- MV.unsafeRead copy (i + offset)
      MV.unsafeWrite ps i program
    forM_ [x..MV.length ps - 1] $ \i -> do
      program <- MV.unsafeRead copy (i - x)
      MV.unsafeWrite ps i program

danceExchange :: STVector s Char -> Int -> Int -> ST s ()
danceExchange ps indexA indexB =
  MV.unsafeSwap ps indexA indexB

dancePartner :: STVector s Char -> Char -> Char -> ST s ()
dancePartner ps programA programB = do
  ps' <- V.freeze ps
  let (indexA, indexB) =
        fromMaybe (0,0) $ do
          indexA <- V.elemIndex programA ps'
          indexB <- V.elemIndex programB ps'
          return $ (indexA, indexB)
  MV.unsafeSwap ps indexA indexB

danceMove :: STVector s Char -> Move -> ST s ()
danceMove ps (Spin x) = danceSpin ps x
danceMove ps (Exchange indexA indexB) = danceExchange ps indexA indexB
danceMove ps (Partner programA programB) = dancePartner ps programA programB

dance :: Programs -> [Move] -> Programs
dance = dances 1

dances :: Int -> Programs -> [Move] -> Programs
dances n ps moves =
  V.modify (\mv -> replicateM_ n $ forM_ moves (danceMove mv)) ps
