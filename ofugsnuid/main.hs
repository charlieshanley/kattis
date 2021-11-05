
main :: IO ()
main = interact $ unlines . rev . drop 1 . lines

rev :: [a] -> [a]
rev = foldl (flip (:)) []
