{-# OPTIONS -Wall #-}

main :: IO ()
main = do
  a <- readLn
  b <- readLn
  let go = do
        c <- readLn
        print $ solveSquare a b c
  go

-- | Solve equation of kind ax² + bx + c = 0
solveSquare :: Double -> Double -> Double -> [Double]
solveSquare a b c
  | a == 0 && b == 0  = []
  | a == 0            = [- c / b]
  | d < 0             = []
  | otherwise         = [ (- b +- sqrt d) / (2 * a)
                        | (+-) <- [(-), (+)] 
                        ]
  where
    d = b * b - 4 * a * c
