{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
    [altitude, degreesOffHorizon] <- fmap read . words <$> getContents
    putStrLn . output $ deathIn degreesOffHorizon altitude

deathIn :: Int -> Int -> Maybe Int
deathIn degreesOffHorizon _ | degreesOffHorizon <= 180 = Nothing
deathIn degreesOffHorizon altitude =
    Just . floor @Double $ fromIntegral altitude / cos radiansOffDown
  where
    degreesOffDown = abs $ degreesOffHorizon - 270
    radiansOffDown = fromIntegral degreesOffDown / 360 * 2 * pi

output :: Maybe Int -> String
output Nothing = "safe"
output (Just s) = show s
