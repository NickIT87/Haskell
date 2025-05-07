-- Файл: sumWithDollar.hs

-- Определим модуль (необязательно)
module Main where

myDollar :: (a -> b) -> a -> b
myDollar = id
-- myDollar f = f
-- myDollar f x = f x

-- Функция сложения
add :: Int -> Int -> Int
add x y = x + y

-- Главная функция
main :: IO ()
main = do
    let a = 5
    let b = 7
    -- Используем оператор $ чтобы избежать скобок
    print $ add a b
    print $ myDollar (add 3) 4  -- => 7
