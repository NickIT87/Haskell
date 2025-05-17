module Main where

import Control.Monad (foldM)
import Data.List (intercalate)

-- Удаляет первое нечётное число
removeFirstOdd :: [Int] -> IO [Int]
removeFirstOdd xs = do
  let result = removeFirstOddPure xs
  logStep "removeFirstOdd" xs result
  return result

-- Удаляет первое чётное число
removeFirstEven :: [Int] -> IO [Int]
removeFirstEven xs = do
  let result = removeFirstEvenPure xs
  logStep "removeFirstEven" xs result
  return result

-- Логика без IO (для удобства повторного использования)
removeFirstOddPure :: [Int] -> [Int]
removeFirstOddPure [] = []
removeFirstOddPure (x:xs)
  | odd x     = xs
  | otherwise = x : removeFirstOddPure xs

removeFirstEvenPure :: [Int] -> [Int]
removeFirstEvenPure [] = []
removeFirstEvenPure (x:xs)
  | even x    = xs
  | otherwise = x : removeFirstEvenPure xs

-- Логирует шаг
logStep :: String -> [Int] -> [Int] -> IO ()
logStep fname before after = do
  putStrLn $ fname ++ ":"
  putStrLn $ "  До:  " ++ show before
  if before == after
    then putStrLn "  (без изменений)"
    else putStrLn $ "  После: " ++ show after

-- Применяет все функции по очереди с логированием
applyAll :: [[Int] -> IO [Int]] -> [Int] -> IO [Int]
applyAll fs x = foldM (\acc f -> f acc) x fs

-- Главный цикл: повторяет применение, пока список меняется
fixPointMany :: [[Int] -> IO [Int]] -> [Int] -> IO [Int]
fixPointMany fs xs = do
  xs' <- applyAll fs xs
  if xs == xs'
    then return xs
    else do
      putStrLn "-- Повторяем цикл --"
      fixPointMany fs xs'

-- Обрабатывает список с логированием
processList :: [Int] -> IO [Int]
processList = fixPointMany [removeFirstOdd, removeFirstEven]

-- Точка входа
main :: IO ()
main = do
  let input = [2, 3, 4, 5, 6, 7, 8]
  putStrLn $ "Исходный список: " ++ show input
  putStrLn "=== Начинаем обработку ==="
  result <- processList input
  putStrLn "=== Завершено ==="
  putStrLn $ "Результат: " ++ show result
