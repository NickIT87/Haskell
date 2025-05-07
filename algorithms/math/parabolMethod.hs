-- Пример: минимизировать f(x) = (x - 2)^2 + 1

f :: Double -> Double
f x = (x - 2)^2 + 1

-- Вычисление вершины параболы через три точки (x1, x2, x3)
parabolaMinimum :: (Double, Double, Double) -> Double
parabolaMinimum (x1, x2, x3) =
  let f1 = f x1
      f2 = f x2
      f3 = f x3
      numerator = (x2 - x1)^2 * (f2 - f3) - (x2 - x3)^2 * (f2 - f1)
      denominator = (x2 - x1)*(f2 - f3) - (x2 - x3)*(f2 - f1)
  in x2 - 0.5 * numerator / denominator

main :: IO ()
main = do
  let xMin = parabolaMinimum (1.0, 1.5, 3.0)
  putStrLn $ "Approximate minimum at x = " ++ show xMin
  putStrLn $ "f(x) = " ++ show (f xMin)
