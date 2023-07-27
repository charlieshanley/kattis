-- https://open.kattis.com/problems/honey
--
-- A bee larva living in a hexagonal cell of a large honeycomb decides to creep
-- for a walk. In each “step” the larva may move into any of the six adjacent
-- cells and after n steps, it is to end up in its original cell.
-- Your program has to compute, for a given n, the number of different such larva walks.


{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Data.Bifunctor     (bimap, first)
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)

main :: IO ()
main = interact $ unlines . fmap (show . nPaths . read) . drop 1 . lines

----------

data Direction = N | NE | SE | S | SW | NW
    deriving (Eq, Show, Enum, Bounded)

type Position = (Int, Int)
type NSteps = Int

nPaths :: NSteps -> Int
nPaths n = pathMap Map.! ((0, 0), n)

minStepsHome :: Position -> NSteps
minStepsHome position = eastWest + (max 0 (northSouth - eastWest) `div` 2)
  where (northSouth, eastWest) = bimap abs abs position

move :: Direction -> Position -> Position
move N  = first $ succ.succ
move NE = bimap succ succ
move SE = bimap pred succ
move S  = first $ pred.pred
move SW = bimap pred pred
move NW = bimap succ pred

directions :: [Direction]
directions = [minBound .. maxBound]

pathMap :: Map (Position, NSteps) Int
pathMap = flip Map.fromSet keys $ \case
    ((0, 0), 0) -> 1
    (pos, nSteps)
      | minStepsHome pos > nSteps -> 0
      | otherwise -> sum $ do
          direction <- directions
          let pos' = move direction pos
          pure $ pathMap Map.! (pos', pred nSteps)

-- larger than it needs to be, but no matter
keys :: Set (Position, NSteps)
keys = Set.fromList $ do
    n <- [-maxPath*2 .. maxPath*2]
    e <- [-maxPath .. maxPath]
    nSteps <- [0 .. maxPath]
    pure ((n, e), nSteps)

maxPath :: NSteps
maxPath = 20
