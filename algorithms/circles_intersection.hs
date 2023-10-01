distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt((x1 - x2)^2 + (y1 - y2)^2)

intersectionPoints :: Float -> Float -> Float -> Float -> Float -> Float -> Int
intersectionPoints x1 y1 r1 x2 y2 r2
    | d > r1 + r2 = 0  -- Окружности не пересекаются
    | d < abs (r1 - r2) = 0  -- Одна окружность внутри другой, нет пересечений
    | d == 0 && r1 == r2 = -1  -- Окружности совпадают, бесконечно много пересечений
    | d == r1 + r2 || d == abs (r1 - r2) = 1  -- Окружности касаются друг друга в одной точке
    | otherwise = 2  -- Окружности пересекаются в двух точках
    where
        d = distance x1 y1 x2 y2

main :: IO ()
main = do
    putStrLn "Введите x1 y1 r1 x2 y2 r2:"
    input <- getLine
    let [x1, y1, r1, x2, y2, r2] = map read (words input) :: [Float]
    let result = intersectionPoints x1 y1 r1 x2 y2 r2
    putStrLn $ "Количество точек пересечения: " ++ show result
