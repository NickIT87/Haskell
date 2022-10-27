{-# OPTIONS -Wall #-}

main :: IO ()
main =
  print solution
  where 
    solution = fac 3


fac :: (Ord t, Num t) => t -> t
fac n = 
    if n <= 1 then
      1
    else
      n * fac (n-1)

{- ghcid -a filename -}
-- $> main