-- Get the first n elements of a list
takeN :: Int -> [a] -> [a]
takeN n _      | n <= 0 = []
takeN _ []              = []
takeN n (x:xs)          = x : takeN (n-1) xs

-- Remove the first n elements of a list
dropN :: Int -> [a] -> [a]
dropN n xs     | n <= 0 = xs
dropN _ []              = []
dropN n (_:xs)          = dropN (n-1) xs

-- Reverse a list
reverseList :: [a] -> [a]
reverseList xs = rev xs []
  where
    rev []     ys = ys
    rev (y:ys) zs = rev ys (y:zs)

main :: IO ()
main = do
  let xs = [1, 2, 3, 4, 5]
  putStrLn ("The first 3 elements of " ++ show xs ++ " are: " ++ show (takeN 3 xs))
  putStrLn ("After removing the first 2 elements of " ++ show xs ++ ", we get: " ++ show (dropN 2 xs))
  putStrLn ("The reverse of " ++ show xs ++ " is: " ++ show (reverseList xs))
