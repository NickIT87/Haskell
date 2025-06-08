{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.IO as T

fixUrl :: String -> String
fixUrl url
  | "http://" `prefix` url || "https://" `prefix` url = url
  | otherwise = "https://" ++ url
  where
    prefix p s = take (length p) s == p

main :: IO ()
main = do
    putStrLn "Enter a URL:"
    rawUrl <- getLine
    let url = fixUrl rawUrl
    request <- parseRequest url
    response <- httpLBS request
    let html = getResponseBody response
    BL8.putStrLn html

    -- Парсим HTML и выводим содержимое <title>
    let cursor = fromDocument $ parseLBS html
        titles = cursor $// element "title" &// content
    putStrLn "\nTitles found:"
    if null titles
      then putStrLn "No <title> found."
      else mapM_ T.putStrLn titles
