pascal :: [[Integer]]
pascal = iterate nextRow [1]
  where
    nextRow row = zipWith (+) (0:row) (row ++ [0])

-- Print the first 10 rows of Pascal's Triangle
main :: IO ()
main = mapM_ print $ take 10 pascal
