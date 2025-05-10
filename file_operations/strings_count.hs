import System.IO

main :: IO ()
main = do
    contents <- readFile "sample.txt"
    let wordCount = length (words contents)
    let lineCount = length (lines contents)
    let charCount = length contents
    putStrLn $ "Number of words: " ++ show wordCount
    putStrLn $ "Number of strings: " ++ show lineCount
    putStrLn $ "Number of symbols: " ++ show charCount