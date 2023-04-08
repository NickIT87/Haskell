quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger  = [y | y <- xs, y > x]

main :: IO ()
main = do
  let xs = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
  putStrLn ("Sorting " ++ show xs ++ " gives: " ++ show (quickSort xs))
