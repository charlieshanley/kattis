{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow  ((>>>))
import Control.Monad  (replicateM, guard)
import Data.Bifunctor (bimap, first, second)
import Data.Foldable  (foldl')

main :: IO ()
main = interact $ lines >>> drop 1 >>> fmap (show . possibilities . read) >>> unlines

-- naively
possibilities :: Int -> Int
possibilities = length . validPaths (0, 0)

data Direction = N | NE | SE | S | SW | NW
    deriving (Eq, Show, Enum, Bounded)

type Position = (Int, Int)

minStepsHome :: Position -> Int
minStepsHome = bimap abs abs >>> \(n, e) -> e + (max 0 (n - e) `div` 2)

validPaths :: Position -> Int -> [[(Direction, Position)]]
validPaths (0, 0) 0         = pure []
validPaths pos    stepsLeft = do
    direction <- universe
    let pos' = move direction pos
    guard $ minStepsHome pos' <= stepsLeft
    ((direction, pos') :) <$> validPaths pos' (pred stepsLeft)

move :: Direction -> Position -> Position
move N  = first $ succ.succ
move NE = bimap succ succ
move SE = bimap pred succ
move S  = first $ pred.pred
move SW = bimap pred pred
move NW = bimap succ pred

universe :: ( Bounded a, Enum a ) => [a]
universe = [minBound .. maxBound]
