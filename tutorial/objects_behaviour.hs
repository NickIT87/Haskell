{-# LANGUAGE OverloadedStrings #-}

class Car a where
  startEngine :: a -> String
  honk        :: a -> String
  describe    :: a -> String

data Toyota = Toyota { model :: String }
data BMW    = BMW    { series :: String, turbo :: Bool }

instance Car Toyota where
  startEngine _ = "Toyota engine started quietly."
  honk _        = "Toyota: beep-beep!"
  describe t    = "Toyota model: " ++ model t

instance Car BMW where
  startEngine b = "BMW roars to life with a " ++ if turbo b then "turbo boost!" else "smooth hum."
  honk _        = "BMW: HOOONK!"
  describe b    = "BMW Series: " ++ series b ++ (if turbo b then " (Turbo)" else "")

testDrive :: Car a => a -> IO ()
testDrive car = do
  putStrLn (describe car)
  putStrLn (startEngine car)
  putStrLn (honk car)

main :: IO ()
main = do
  let corolla = Toyota "Corolla"
      m3      = BMW "M3" True

  putStrLn "=== Test Driving Corolla ==="
  testDrive corolla

  putStrLn "\n=== Test Driving BMW M3 ==="
  testDrive m3
