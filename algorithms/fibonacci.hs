fibonacci :: Int -> Int
fibonacci n = if n <= 1 then n else fibonacci (n-1) + fibonacci (n-2)

main :: IO ()
main = do
  putStrLn "Enter the number of terms:"
  n <- readLn
  let fibs = map fibonacci [0..n-1]
  putStrLn ("The first " ++ show n ++ " Fibonacci numbers are: " ++ show fibs)
