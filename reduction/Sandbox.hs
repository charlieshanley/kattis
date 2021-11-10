
module Sandbox where

initial, target, predPrice, halvePrice :: Int
initial = 100
target = 5
predPrice = 0
halvePrice = 1


breakeven = 2 * fromIntegral halvePrice / fromIntegral predPrice
stopHalving = max (2 * fromIntegral target) breakeven
halves = ceiling $ max 0 $ log (fromIntegral initial / stopHalving) / log 2
remainderAfterHalving = initial `div` 2^halves
preds = remainderAfterHalving - target

cost = halves * halvePrice * preds * predPrice
