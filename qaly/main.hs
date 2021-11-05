{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = interact $
  show . sum . fmap (product . fmap (read @Double) . words) . drop 1 . lines
