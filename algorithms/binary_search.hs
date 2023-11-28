binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch [] _ = Nothing
binarySearch xs target = binarySearch' xs target 0 (length xs - 1)
  where
    binarySearch' :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
    binarySearch' xs target low high
      | low > high = Nothing
      | otherwise = 
          let mid = (low + high) `div` 2
              midValue = xs !! mid
          in case compare midValue target of
               EQ -> Just mid
               LT -> binarySearch' xs target (mid + 1) high
               GT -> binarySearch' xs target low (mid - 1)

-- Example usage:
main :: IO ()
main = do
  let myList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  putStrLn "Enter a number to search:"
  input <- getLine
  let target = read input :: Int
  case binarySearch myList target of
    Just index -> putStrLn $ show target ++ " found at index " ++ show index
    Nothing    -> putStrLn $ show target ++ " not found in the list"
