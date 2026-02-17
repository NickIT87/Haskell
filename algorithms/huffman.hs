-- HuffmanFinal.hs
-- Финальная версия без специальных символов

module Main where

-- Дерево Хаффмана
data Tree = Leaf Char Int | Node Int Tree Tree

-- Для вывода дерева
instance Show Tree where
    show (Leaf c w) = "(" ++ [c] ++ ":" ++ show w ++ ")"
    show (Node w l r) = "(" ++ show w ++ " " ++ show l ++ " " ++ show r ++ ")"

-- Вес узла
weight :: Tree -> Int
weight (Leaf _ w) = w
weight (Node w _ _) = w

-- Вставка в отсортированный список
insert :: Tree -> [Tree] -> [Tree]
insert t [] = [t]
insert t (x:xs) = if weight t <= weight x then t:x:xs else x:insert t xs

-- Построение дерева
build :: [(Char, Int)] -> Tree
build [(c,f)] = Leaf c f
build xs = build' (foldr insert [] [Leaf c f | (c,f) <- xs])
  where
    build' [t] = t
    build' (t1:t2:ts) = build' $ insert (Node (weight t1 + weight t2) t1 t2) ts

-- Получение кодов
getCodes :: Tree -> [(Char, String)]
getCodes t = getCodes' t ""
  where
    getCodes' (Leaf c _) code = [(c, code)]
    getCodes' (Node _ l r) code = getCodes' l (code ++ "0") ++ getCodes' r (code ++ "1")

-- Поиск кода по символу
findCode :: [(Char, String)] -> Char -> String
findCode [] _ = ""
findCode ((c,code):xs) ch = if c == ch then code else findCode xs ch

-- Кодирование
encode :: String -> [(Char, String)] -> String
encode s codes = concatMap (findCode codes) s

-- Декодирование
decode :: String -> Tree -> String
decode bits tree = decode' bits tree
  where
    decode' [] _ = []
    decode' bs node = go bs node
      where
        go [] (Leaf c _) = [c]  -- Дочитали до конца на листе
        go (b:bs) (Node _ l r)
            | b == '0' = go bs l
            | b == '1' = go bs r
        go bs (Leaf c _) = c : decode' bs tree  -- Нашли лист, продолжаем

-- Подсчет частот
countFreq :: String -> [(Char, Int)]
countFreq s = [(c, length (filter (==c) s)) | c <- nub s]
  where
    nub [] = []
    nub (x:xs) = x : nub (filter (/=x) xs)

-- Проверка работы
test :: String -> IO ()
test s = do
    putStrLn $ "Текст: \"" ++ s ++ "\""
    
    let freqs = countFreq s
    putStrLn "Частоты:"
    mapM_ (\(c,n) -> putStrLn $ "  '" ++ [c] ++ "': " ++ show n) freqs
    
    let tree = build freqs
    let codes = getCodes tree
    putStrLn "Коды:"
    mapM_ (\(c,code) -> putStrLn $ "  '" ++ [c] ++ "': " ++ code) codes
    
    let encoded = encode s codes
    putStrLn $ "Закодировано: " ++ encoded
    
    let decoded = decode encoded tree
    putStrLn $ "Декодировано: \"" ++ decoded ++ "\""
    
    if s == decoded 
        then putStrLn "Результат: УСПЕШНО" 
        else putStrLn $ "Результат: ОШИБКА (получено \"" ++ decoded ++ "\")"
    
    let originalBits = length s * 8
        compressedBits = length encoded
    putStrLn $ "Размер: " ++ show originalBits ++ " бит -> " ++ show compressedBits ++ " бит"
    putStrLn $ "Сжатие: " ++ show (100 * fromIntegral compressedBits / fromIntegral originalBits) ++ "%"
    putStrLn "------------------------"

main :: IO ()
main = do
    putStrLn "=== АЛГОРИТМ ХАФФМАНА ==="
    putStrLn "1. Тест на 'asdfghf'"
    putStrLn "2. Тест на 'hello world'"
    putStrLn "3. Ввести свой текст"
    putStrLn "4. Выход"
    putStr "Выберите опцию (1-4): "
    
    choice <- getLine
    case choice of
        "1" -> test "asdfghf" >> main
        "2" -> test "hello world" >> main
        "3" -> do
            putStr "Введите текст: "
            text <- getLine
            if null text
                then putStrLn "Текст не может быть пустым!" >> main
                else test text >> main
        "4" -> putStrLn "До свидания!"
        _ -> do
            putStrLn "Неверный выбор!"
            main