
runGame :: IO ()
runGame = do
  let secretNumber = "5"
  putStrLn  "Enter a guess between 1 and 10"
  userGuess <- getLine

  if userGuess == secretNumber 
  then putStrLn  "You win!"
  else runGame


main :: IO ()
main = do
  runGame