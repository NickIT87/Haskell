selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []  -- пустий список вже відсортований
{--
Якщо вхідний список xs не порожній, то функція складає 
мінімальний елемент minimumElement із невідсортованим 
залишком selectionSort remaining. Тут minimumElement - це 
мінімальний елемент вихідного списку, а remaining - список, 
з якого вилучено цей мінімальний елемент.
--}
selectionSort xs = minimumElement : selectionSort remaining
  where
    {-- 
    Знаходження мінімального елемента у вхідному списку
    xs за допомогою функції minimum.
    --}
    minimumElement = minimum xs
    {-- 
    filter (/= minimumElement) xs - це застосування фільтра
    до списку xs, де кожен елемент перевіряється на 
    нерівність з minimumElement.
    --}
    remaining = filter (/= minimumElement) xs

-- Приклад використання:
main :: IO ()
main = do
  let unsortedList = [4, 2, 7, 1, 9, 5, 3]
  putStrLn "Unsorted List:"
  print unsortedList

  let sortedList = selectionSort unsortedList
  putStrLn "Sorted List:"
  print sortedList


-- selectionSort' :: (Ord a) => [a] -> [a]
-- selectionSort' [] = []
-- selectionSort' xs = minimumElement : selectionSort remaining
--   where
--     minimumElement = minimum xs
--     remaining = filter (/= minimumElement) xs
