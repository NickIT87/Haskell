import Data.List (sort)

type Interval = (Double, Double)
type Sign = Ordering  -- LT, EQ, GT

-- Критические точки: нули числителя и знаменателя
criticalPoints :: [Double]
criticalPoints = [-2, 1, 3]

-- Точки, при которых функция не определена (знаменатель = 0)
undefinedPoints :: [Double]
undefinedPoints = [3]

-- Функция
f :: Double -> Double
f x = ((x - 1) * (x + 2)) / (x - 3)

-- Создание интервалов
makeIntervals :: [Double] -> [Interval]
makeIntervals xs = zip xs' (drop 1 xs')  -- заменили tail на drop 1
  where xs' = (-1/0) : xs ++ [1/0]  -- от -∞ до ∞

-- Определить знак функции на интервале
signOnInterval :: Interval -> Sign
signOnInterval (a, b) = compare (f mid) 0
  where mid = (a + b) / 2

-- Оформить интервал в виде строки
formatInterval :: Interval -> String
formatInterval (a, b) = 
  let left = if isInfinite a then "(-Infinity" else "(" ++ show a
      right = if isInfinite b then "Infinity)" else show b ++ ")"
  in left ++ ", " ++ right

-- Проверка на исключение интервала с точкой разрыва
excludeUndefined :: Interval -> Bool
excludeUndefined (a, b) = not (a == 3 || b == 3)  -- Исключаем интервалы, содержащие x = 3

-- Основная логика
main :: IO ()
main = do
  let sortedPoints = sort (criticalPoints ++ undefinedPoints)
      intervals = makeIntervals sortedPoints
      signs = map (\i -> (i, signOnInterval i)) intervals
      positiveIntervals = [formatInterval i | (i, GT) <- signs, excludeUndefined i]  -- Исключаем интервалы с x = 3

  putStrLn "Решение неравенства f(x) > 0:"
  putStrLn $ concat $ joinWith " U " positiveIntervals  -- заменили unwords на concat

-- Вспомогательная функция объединения строк
joinWith :: String -> [String] -> [String]
joinWith _   []     = []
joinWith _   [x]    = [x]
joinWith sep (x:xs) = x : sep : joinWith sep xs
