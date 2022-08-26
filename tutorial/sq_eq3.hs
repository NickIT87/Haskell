{-# OPTIONS -Wall -Werror #-}

main :: IO ()
main
  | a == 0 =
    -- b x + c = 0
    if b == 0 then
      -- c = 0
      if c == 0 then
        -- 0 = 0
        putStrLn "Many soltions"
      else
        -- 0 /= 0
        putStrLn "No soltions"
    else
        print (- c / b)
  | d < 0     = putStrLn "no solutions"
  | otherwise = print (x1, x2)    
  where
    a = 0 :: Double
    b = 2.5
    c = 1
    d = b * b - 4 * a * c
    x1 = (- b - sqrt d) / (2 * a)
    x2 = (- b + sqrt d) / (2 * a)

-- $> main