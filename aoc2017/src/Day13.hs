{-# LANGUAGE NamedFieldPuns #-}
module Day13 where

import Data.Bifunctor (second)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Text.Megaparsec
import Text.Megaparsec.Char

data Dir = Up | Down
  deriving (Show)

data Layer = Layer
  { range :: Int
  , scannerPosition :: Int
  , scannerDirection :: Dir
  }

instance Show Layer where
  show layer = layerToString False layer

layerToString :: Bool -> Layer -> String
layerToString packetIsHere (Layer { range, scannerPosition }) =
    unwords .
    map
     (\i ->
        let content =
              if i == scannerPosition then "S"
              else " "
        in if packetIsHere && i == 0 then
          "(" ++ content ++ ")"
        else
          "[" ++ content ++ "]"
     ) $
    [0..range-1]

initLayer :: Int -> Layer
initLayer range =
  Layer { range, scannerPosition = 0, scannerDirection = Down }

data Firewall = Firewall
  { layers :: IntMap Layer
  , packetDepth :: Int
  , tripSeverity :: Int
  }

instance Show Firewall where
  show (Firewall { layers, packetDepth }) =
    let (maxDepth, _) = M.findMax layers
    in
    unlines .
    map
      (\depth ->
         show depth ++ " " ++
         case M.lookup depth layers of
           Just layer -> layerToString (depth == packetDepth) layer
           Nothing ->
             if depth == packetDepth then
               "(.)"
             else
               "..."
         ) $
    [0..maxDepth]

initFirewall :: [(Int, Int)] -> Firewall
initFirewall layerDepths =
  Firewall
    { layers =
        M.fromList . map (second initLayer) $ layerDepths
    , packetDepth = -1
    , tripSeverity = 0
    }

step :: Firewall -> Firewall
step (Firewall {layers, packetDepth, tripSeverity}) =
  let packetDepth' = packetDepth + 1
      tripSeverity' =
        case M.lookup packetDepth' layers of
          Nothing -> tripSeverity
          Just layer ->
            let caught = scannerPosition layer == 0
            in if caught then
                 tripSeverity + packetDepth' * (range layer)
            else
              tripSeverity
  in Firewall
      { layers = stepScanners layers
      , packetDepth = packetDepth'
      , tripSeverity = tripSeverity'
      }

stepScanners :: IntMap Layer -> IntMap Layer
stepScanners layers =
  fmap
    (\ Layer {range, scannerPosition, scannerDirection} ->
       case scannerDirection of
         Down ->
           if scannerPosition == range - 1 then
             Layer {range, scannerPosition = scannerPosition - 1, scannerDirection = Up}
           else
             Layer {range, scannerPosition = scannerPosition + 1, scannerDirection}
         Up ->
           if scannerPosition == 0 then
             Layer {range, scannerPosition = scannerPosition + 1, scannerDirection = Down}
           else
             Layer {range, scannerPosition = scannerPosition - 1, scannerDirection}
       )
  layers

crossFirewall :: Firewall -> [Firewall]
crossFirewall =
  takeWhile (\fw ->
    let (maxDepth, _) = M.findMax (layers fw)
    in packetDepth fw <= maxDepth) .
  iterate step

severity :: Firewall -> Int
severity = tripSeverity . last . crossFirewall

parseInput :: String -> Either (ParseError (Token String) ()) Firewall
parseInput input =
  parse parser "" input
  where
    entry :: Parsec () String (Int, Int)
    entry = do
      k <- read <$> some digitChar
      _ <- string ": "
      v <- read <$> some digitChar
      return (k, v)
    parser :: Parsec () String Firewall
    parser =
      initFirewall <$> sepEndBy entry eol

canPass :: [IntMap Layer] -> Bool
canPass layerStates =
  all (\(i, layers) ->
           case M.lookup i layers of
             Nothing -> True
             Just (Layer { scannerPosition }) ->
               scannerPosition /= 0
           ) .
  zip [0..] $ layerStates

findDelay :: Firewall -> Int
findDelay (Firewall { layers }) =
  let (maxDepth, _) = M.findMax layers
      layerStates = take (maxDepth + 1) . iterate stepScanners $ layers
  in
    go 0 (last layerStates) layerStates
    where go i lastLayers layerStates =
            if canPass layerStates then i
            else
              let layers' = stepScanners lastLayers
              in go (i + 1) layers' (drop 1 layerStates ++ [layers'])
