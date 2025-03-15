{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)

main :: IO()
main = scotty 3000 $ do
    get "/" $ do
        text "Hello Earth"

    get "/:word" $ do
        beam <- pathParam "word"
        html $  mconcat ["<h1>Scotty, " , beam, "</h>"]
