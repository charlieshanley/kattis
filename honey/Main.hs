{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow      ((>>>))
import Control.Monad      (replicateM, guard)
import Data.Bifunctor     (bimap, first, second)
import Data.Foldable      (foldl')
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)

main :: IO ()
main = interact $ lines >>> drop 1 >>> fmap (show . nPaths . read) >>> unlines

----------

data Direction = N | NE | SE | S | SW | NW
    deriving (Eq, Show, Enum, Bounded)

type Position = (Int, Int)
type NSteps = Int

nPaths :: NSteps -> Int
nPaths n = pathMap Map.! ((0, 0), n)

minStepsHome :: Position -> NSteps
minStepsHome = bimap abs abs >>> \(n, e) -> e + (max 0 (n - e) `div` 2)

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
