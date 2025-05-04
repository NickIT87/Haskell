{-# LANGUAGE OverloadedStrings #-}

-- Объявляем классы
class Vehicle a where
  describe :: a -> String
  startEngine :: a -> String

class Vehicle a => Car a where
  honk :: a -> String

class Vehicle a => Truck a where
  loadCargo :: a -> Int -> String

-- Модели
data Toyota = Toyota { model :: String }
data VolvoTruck = VolvoTruck { capacity :: Int }

-- Реализация Vehicle и Car для Toyota
instance Vehicle Toyota where
  describe t = "Toyota model: " ++ model t
  startEngine _ = "Toyota engine started quietly."

instance Car Toyota where
  honk _ = "Toyota: beep-beep!"

-- Реализация Vehicle и Truck для VolvoTruck
instance Vehicle VolvoTruck where
  describe _ = "Volvo heavy-duty truck"
  startEngine _ = "VolvoTruck engine started with diesel rumble."

instance Truck VolvoTruck where
  loadCargo truck weight =
    if weight <= capacity truck
      then "Cargo loaded: " ++ show weight ++ "kg"
      else "Over capacity! Max: " ++ show (capacity truck) ++ "kg"

-- Функции, работающие с любыми Car или Truck
testDriveCar :: Car a => a -> IO ()
testDriveCar car = do
  putStrLn (describe car)
  putStrLn (startEngine car)
  putStrLn (honk car)

testDriveTruck :: Truck a => a -> IO ()
testDriveTruck truck = do
  putStrLn (describe truck)
  putStrLn (startEngine truck)
  putStrLn (loadCargo truck 5000)

-- Главная функция
main :: IO ()
main = do
  let corolla = Toyota "Corolla"
      volvo   = VolvoTruck 8000

  putStrLn "=== Driving Car ==="
  testDriveCar corolla

  putStrLn "\n=== Driving Truck ==="
  testDriveTruck volvo
