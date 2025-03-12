module Geometry where

type Point = (Double, Double)
type Vector = (Double, Double)

-- Сложение векторов
addVectors :: Vector -> Vector -> Vector
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Вычитание векторов
subVectors :: Vector -> Vector -> Vector
subVectors (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Умножение вектора на скаляр
scaleVector :: Double -> Vector -> Vector
scaleVector k (x, y) = (k * x, k * y)

-- Скалярное произведение
dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- Длина вектора
vectorLength :: Vector -> Double
vectorLength (x, y) = sqrt (x * x + y * y)

-- Нормализация вектора
normalizeVector :: Vector -> Vector
normalizeVector v@(x, y) = 
    let len = vectorLength v
    in if len == 0 then (0, 0) else (x / len, y / len)

-- Расстояние между двумя точками
distance :: Point -> Point -> Double
distance p1 p2 = vectorLength (subVectors p1 p2)

-- Смещение точки на вектор
movePoint :: Point -> Vector -> Point
movePoint = addVectors

-- Поворот точки вокруг начала координат
rotatePoint :: Double -> Point -> Point
rotatePoint theta (x, y) =
    (x * cos theta - y * sin theta, x * sin theta + y * cos theta)

-- Пример использования
main :: IO ()
main = do
    let v1 = (3, 4) :: Vector
        v2 = (1, 2) :: Vector
        p1 = (2, 3) :: Point
        p2 = (5, 7) :: Point
        theta = pi / 4  -- Поворот на 45 градусов
    
    putStrLn $ "Сложение векторов: " ++ show (addVectors v1 v2)
    putStrLn $ "Разность векторов: " ++ show (subVectors v1 v2)
    putStrLn $ "Длина вектора v1: " ++ show (vectorLength v1)
    putStrLn $ "Нормализованный v1: " ++ show (normalizeVector v1)
    putStrLn $ "Скалярное произведение v1 и v2: " ++ show (dotProduct v1 v2)
    putStrLn $ "Расстояние между p1 и p2: " ++ show (distance p1 p2)
    putStrLn $ "Поворот точки p1 на 45 градусов: " ++ show (rotatePoint theta p1)