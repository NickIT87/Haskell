main :: IO ()

rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

main = do
  let message = "hello world"
  putStrLn message
  print rightTriangles'
