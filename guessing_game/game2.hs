
runGame :: Int -> IO ()
runGame incorrectGuesses = do
  let secretNumber = "5"

  if incorrectGuesses == 3
  then putStrLn "Sorry, you lose :("
  else do
    putStrLn "Enter a guess between 1 and 10"
    userGuess <- getLine
    if userGuess == secretNumber
    then putStrLn "Yay, you win!"
    else runGame (incorrectGuesses + 1)


main :: IO ()
main = do
  runGame 0