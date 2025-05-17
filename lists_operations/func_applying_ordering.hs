module Main where

-- Удаляет первое нечётное число из списка
removeFirstOdd :: [Int] -> [Int]
removeFirstOdd [] = []
removeFirstOdd (x:xs)
  | odd x     = xs
  | otherwise = x : removeFirstOdd xs

-- Удаляет первое чётное число из списка
removeFirstEven :: [Int] -> [Int]
removeFirstEven [] = []
removeFirstEven (x:xs)
  | even x    = xs
  | otherwise = x : removeFirstEven xs

-- Применяет список функций по очереди к значению
applyAll :: Eq a => [a -> a] -> a -> a
applyAll fs x = foldl (\acc f -> f acc) x fs

-- Повторяет применение функций, пока результат меняется
fixPointMany :: Eq a => [a -> a] -> a -> a
fixPointMany fs x =
  let x' = applyAll fs x
  in if x == x' then x else fixPointMany fs x'

-- Основная функция обработки списка
processList :: [Int] -> [Int]
processList = fixPointMany [removeFirstOdd, removeFirstEven]

-- Точка входа (для теста)
main :: IO ()
main = do
  let input = [2, 3, 4, 5, 6, 7, 8]
  let output = processList input
  putStrLn $ "Исходный список: " ++ show input
  putStrLn $ "Результат после обработки: " ++ show output
