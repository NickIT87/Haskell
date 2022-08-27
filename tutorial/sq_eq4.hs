{-# OPTIONS -Wall #-}

main :: IO ()
main =
  print solution
  where
    a = 1 :: Double
    b = 2
    c = 1
    solution = solveSquare a b c

-- | Solve equation of kind a x^2 + b x + x = 0
solveSquare :: Double -> Double -> Double -> [Double]
solveSquare a b c
  | a == 0 && b == 0  = []
  | a == 0            = [- c / b]
  | d < 0             = []
  | otherwise         = [x1, x2]
  where
    d = b * b - 4 * a * c

    -- | first root
    x1 = (- b - sqrt d) / (2 * a)
    
    x2 = (- b + sqrt d) / (2 * a)
    -- ^ second root

-- $> solveSquare 1 0 1

-- $> solveSquare 1 2 1

-- $> solveSquare 1 (-2) 1
