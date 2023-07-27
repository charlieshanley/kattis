-- https://open.kattis.com/problems/honey
--
-- A bee larva living in a hexagonal cell of a large honeycomb decides to creep
-- for a walk. In each “step” the larva may move into any of the six adjacent
-- cells and after n steps, it is to end up in its original cell.
-- Your program has to compute, for a given n, the number of different such larva walks.


{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import Dict (Dict)
import qualified Dict
import Platform (DoAnythingHandler, doAnything)

import Prelude (interact, lines, unlines, read, show, drop, error)
import NriPrelude hiding (e)

mainTask :: DoAnythingHandler -> Task Never ()
mainTask handler = do
  let io = interact <| unlines << fmap (show << nPaths << read) << drop 1 << lines
  doAnything handler (map Ok io)

----------

data Direction = N | NE | SE | S | SW | NW
    deriving (Eq, Show)

type Position = (Int, Int)
type NSteps = Int

nPaths :: NSteps -> Int
nPaths n = getPartial ((0, 0), n) pathMap

getPartial :: ( Ord k ) => k -> Dict k v -> v
getPartial k dict =
  case Dict.get k dict of
    Just v -> v
    Nothing -> error "pay no attention..."

minStepsHome :: Position -> NSteps
minStepsHome (north, east) =
  let northSouth = abs north
      eastWest = abs east
   in eastWest + (max 0 (northSouth - eastWest) // 2)

move :: Position -> Direction -> Position
move (n, e) N  = (n + 2, e)
move (n, e) NE = (n + 1, e + 1)
move (n, e) SE = (n + 1, e - 1)
move (n, e) S  = (n - 2, e)
move (n, e) SW = (n - 1, e - 1)
move (n, e) NW = (n + 1, e - 1)

directions :: [Direction]
directions = [N, NE, SE, S, SW, NW]

cartesianProduct :: List a -> List b -> List (a, b)
cartesianProduct as bs =
  as |> List.concatMap (\a -> bs |> List.map (\b -> (a, b)))

-- Pretend this is a lazy Dict
pathMap :: Dict (Position, NSteps) Int
pathMap =
  let
    norths = List.range (-maxPath*2) (maxPath*2)
    easts = List.range (-maxPath) (maxPath)
    positions = cartesianProduct norths easts
    nStepss = List.range 0 maxPath
    keys = cartesianProduct positions nStepss
   in
    keys
      |> List.map
        (\(pos :: Position, nSteps :: NSteps) ->
          ( (pos, nSteps)
          , if (pos, nSteps) == ((0, 0), 0)
               then 1
               else if minStepsHome pos > nSteps
                       then 0
                       else
                        directions
                          |> List.map (\direction -> move pos direction)
                          |> List.map (\newPos -> getPartial (newPos, nSteps - 1) pathMap)
                          |> List.sum
          )
        )
    |> Dict.fromList

maxPath :: NSteps
maxPath = 20
