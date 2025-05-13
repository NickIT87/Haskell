import Control.Monad.Trans.State
import Control.Monad.IO.Class

-- Тип состояния: количество запусков двигателя для каждой машины
type CarState = Int

-- Монада для работы с состоянием и IO
type CarM = StateT CarState IO

-- Класс Car
class Car a where
  startEngine :: a -> String
  honk        :: a -> String
  describe    :: a -> String

-- Типы машин
data Toyota = Toyota { model :: String }
data BMW    = BMW    { series :: String, turbo :: Bool }

-- Реализация Car для Toyota
instance Car Toyota where
  startEngine _ = "Toyota engine started quietly."
  honk _        = "Toyota: beep-beep!"
  describe t    = "Toyota model: " ++ model t

-- Реализация Car для BMW
instance Car BMW where
  startEngine b = "BMW roars to life with a " ++ if turbo b then "turbo boost!" else "smooth hum."
  honk _        = "BMW: HOOONK!"
  describe b    = "BMW Series: " ++ series b ++ (if turbo b then " (Turbo)" else "")

-- Функция для заведения двигателя, с изменением состояния
startEngineM :: (Car a) => a -> CarM String
startEngineM car = do
  n <- get  -- Получаем текущий счётчик запусков
  put (n + 1)  -- Увеличиваем счётчик
  return (startEngine car ++ " (запуск №" ++ show (n + 1) ++ ")")

-- Расширенная версия testDrive, использующая StateT
testDriveM :: Car a => a -> CarM ()
testDriveM car = do
  liftIO $ putStrLn (describe car)  -- Печатаем описание машины
  engineMsg <- startEngineM car     -- Заводим двигатель и получаем сообщение
  liftIO $ putStrLn engineMsg       -- Печатаем сообщение о запуске двигателя
  liftIO $ putStrLn (honk car)      -- Печатаем сообщение о гудке

-- Главная функция
main :: IO ()
main = do
  let corolla = Toyota "Corolla"
      m3      = BMW "M3" True

  -- Инициализация состояния для каждой машины
  putStrLn "=== Test Driving Corolla ==="
  (_, corollaState) <- runStateT (testDriveM corolla) 0  -- Начинаем с 0 для Toyota
  putStrLn $ "Всего запусков для Corolla: " ++ show corollaState
  
  putStrLn "\n=== Test Driving BMW M3 ==="
  (_, m3State) <- runStateT (testDriveM m3) 0  -- Начинаем с 0 для BMW
  putStrLn $ "Всего запусков для BMW M3: " ++ show m3State

  -- Теперь запустим двигатель еще раз для обеих машин
  putStrLn "\n=== Test Driving Corolla (еще раз) ==="
  (_, corollaState2) <- runStateT (testDriveM corolla) corollaState  -- Используем состояние от предыдущего запуска
  putStrLn $ "Всего запусков для Corolla после второго запуска: " ++ show corollaState2

  putStrLn "\n=== Test Driving BMW M3 (еще раз) ==="
  (_, m3State2) <- runStateT (testDriveM m3) m3State  -- Используем состояние от предыдущего запуска
  putStrLn $ "Всего запусков для BMW M3 после второго запуска: " ++ show m3State2
