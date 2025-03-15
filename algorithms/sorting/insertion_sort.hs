---- Вставляет элемент в отсортированный список
--insert :: Ord a => a -> [a] -> [a]
--insert x [] = [x]
--insert x (y:ys)
--    | x <= y    = x : y : ys  -- Вставляем перед первым элементом, который больше или равен x
--    | otherwise = y : insert x ys  -- Иначе продолжаем искать место для x
--
---- Рекурсивная сортировка вставками
--insertionSort :: Ord a => [a] -> [a]
--insertionSort [] = []  -- Базовый случай: пустой список уже отсортирован
--insertionSort (x:xs) = insert x (insertionSort xs)  -- Рекурсивно сортируем хвост и вставляем x


insert :: Ord a => a -> [a] -> [a]
insert x xs = let (left, right) = span (< x) xs in left ++ [x] ++ right

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

-- Тестируем сортировку
main :: IO ()
main = print (insertionSort [5, 3, 8, 1, 2])
