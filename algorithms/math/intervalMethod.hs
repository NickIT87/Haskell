type Interval = (Double, Double)
type Sign = Ordering  -- LT, EQ, GT as signs: negative, zero, positive

-- Определим корни числителя и знаменателя
criticalPoints :: [Double]
criticalPoints = [-2, 1, 3]

-- Функция f(x)
f :: Double -> Double
f x = ((x - 1) * (x + 2)) / (x - 3)

-- Интервалы между критическими точками
makeIntervals :: [Double] -> [Interval]
makeIntervals xs = zip xs' (tail xs')
  where xs' = (-1/0) : xs ++ [1/0]  -- -∞ и ∞

-- Вычислить знак функции на каждом интервале
signsOnIntervals :: [Interval] -> [(Interval, Sign)]
signsOnIntervals = map (\(a, b) ->
  let mid = (a + b) / 2
  in ((a, b), compare (f mid) 0))

main :: IO ()
main = do
  let sorted = quicksort criticalPoints
      intervals = makeIntervals sorted
      signs = signsOnIntervals intervals
  mapM_ print signs

-- Простая сортировка
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x]
                 ++ [x]
                 ++ quicksort [y | y <- xs, y > x]
