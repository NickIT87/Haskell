-- Prime factorization function
primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1 = []
  | otherwise = factorize n 2
  where
    factorize :: Integer -> Integer -> [Integer]
    factorize num divisor
      | num <= 1 = []
      | num `mod` divisor == 0 = divisor : factorize (num `div` divisor) divisor
      | otherwise = factorize num (divisor + 1)

-- Example usage
main :: IO ()
main = do
  let numberToFactorize = 84
  putStrLn $ "Prime factors of " ++ show numberToFactorize ++ ": " ++ show (primeFactors numberToFactorize)
