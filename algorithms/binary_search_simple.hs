-- binary_search.hs

-- Binary search for a sorted list.
-- Returns the index of the found element.
-- Returns -1 if the element does not exist.

binarySearch :: (Ord a) => [a] -> a -> Int
binarySearch sortedList targetValue =
  search sortedList 0
  where
    -- Recursive helper function.
    -- currentList: current part of the list
    -- startIndex: original index offset
    search [] _ = -1
    search currentList startIndex
      | middleValue == targetValue =
          startIndex + middleIndex
      | targetValue < middleValue =
          search leftPart startIndex
      | otherwise =
          search rightPart (startIndex + middleIndex + 1)
      where
        -- Find middle index
        middleIndex = length currentList `div` 2

        -- Split list into:
        -- leftPart | middleValue | rightPart
        (leftPart, middleValue : rightPart) =
          splitAt middleIndex currentList

main :: IO ()
main = do
  let numbers = [1, 3, 5, 7, 9, 11, 13]

  print (binarySearch numbers 1)
  print (binarySearch numbers 7)
  print (binarySearch numbers 13)
  print (binarySearch numbers 4)
