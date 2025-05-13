import System.IO

main :: IO ()
main = do
    let content = "Hello, Haskell!\nThis is written to a file."
    writeFile "output.txt" content
    appendFile "output.txt" "Another line!\n"
    -- handle <- openFile "output.txt" WriteMode
    -- hPutStrLn handle "Line 1"
    -- hPutStrLn handle "Line 2"
    -- hClose handle