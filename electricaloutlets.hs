{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = interact $
  unlines . fmap (show . outlets 0 . fmap read . drop 1 . words) . drop 1 . lines

outlets :: Int -> [Int] -> Int
outlets !acc []       = acc
outlets !acc [x]      = acc + x
outlets !acc (x:rest) = outlets (acc + x - 1) rest
