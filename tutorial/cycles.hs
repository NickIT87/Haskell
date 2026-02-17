-- файл: cycles.hs
-- Запуск: runhaskell cycles.hs

-- Самый простой универсальный цикл
loop :: Int -> (a -> a) -> a -> a
loop 0 _ result = result
loop n f result = loop (n-1) f (f result)

-- Пример 1: Арифметика
example1 = do
    putStrLn "=== Пример 1: Арифметика ==="
    let result1 = loop 5 (+2) 0      -- 0 +2 +2 +2 +2 +2 = 10
    putStrLn $ "5 раз прибавить 2 к 0: " ++ show result1
    
    let result2 = loop 3 (*3) 1      -- 1 *3 *3 *3 = 27
    putStrLn $ "3 раза умножить 1 на 3: " ++ show result2
    
    let result3 = loop 10 (+1) 0     -- сумма чисел от 1 до 10
    putStrLn $ "Сумма от 1 до 10: " ++ show result3

-- Пример 2: Работа со строками
example2 = do
    putStrLn "\n=== Пример 2: Работа со строками ==="
    let result = loop 3 (++ "!") "Привет"
    putStrLn $ "Добавить ! 3 раза: " ++ result  -- "Привет!!!"

-- Пример 3: Вывод на экран (специальная версия для IO)
loopPrint :: Int -> IO ()
loopPrint 0 = return ()
loopPrint n = do
    putStrLn $ "Итерация номер " ++ show n
    loopPrint (n-1)

example3 = do
    putStrLn "\n=== Пример 3: Вывод на экран ==="
    loopPrint 3

-- Пример 4: Создание списка
example4 = do
    putStrLn "\n=== Пример 4: Создание списка ==="
    let buildList n = loop n (\xs -> (length xs + 1) : xs) []
    putStrLn $ "Список от 1 до 5: " ++ show (buildList 5)

-- Пример 5: Пользовательский ввод
example5 = do
    putStrLn "\n=== Пример 5: Ввод чисел ==="
    putStrLn "Введите 3 числа (каждое с новой строки):"
    numbers <- readNumbers 3 []
    putStrLn $ "Вы ввели: " ++ show numbers
    putStrLn $ "Сумма: " ++ show (sum numbers)
  where
    readNumbers 0 acc = return acc
    readNumbers n acc = do
        putStr $ "Число " ++ show (4 - n) ++ ": "
        num <- readLn
        readNumbers (n-1) (acc ++ [num])

main :: IO ()
main = do
    putStrLn "Простые примеры циклов в Haskell"
    putStrLn "================================="
    example1
    example2
    example3
    example4
    example5
    putStrLn "\nГотово!"