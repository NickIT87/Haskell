{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Storage
import Logic

import Data.Time (getCurrentTime, utctDay)
import System.Directory (doesFileExist)

dbPath :: FilePath
dbPath = "transactions.json"

main :: IO ()
main = do
  putStrLn "=== Budget Tracker ==="
  exists <- doesFileExist dbPath
  txs <- if exists then loadTransactions dbPath else return []

  putStrLn "1. Add income"
  putStrLn "2. Add expense"
  putStrLn "3. Show balance"
  putStrLn "4. Exit"
  choice <- getLine
  case choice of
    "1" -> addEntry Income txs
    "2" -> addEntry Expense txs
    "3" -> do
      printBalance txs
      main
    "4" -> putStrLn "Goodbye!"
    _   -> main

addEntry :: TransactionType -> [Transaction] -> IO ()
addEntry trType txs = do
  putStrLn "Enter amount:"
  amt <- readLn
  putStrLn "Enter category:"
  cat <- getLine
  putStrLn "Enter comment:"
  com <- getLine
  now <- getCurrentTime
  let tx = Transaction trType amt cat (utctDay now) com
  let newTxs = addTransaction tx txs
  saveTransactions dbPath newTxs
  putStrLn "Saved."
  main

printBalance :: [Transaction] -> IO ()
printBalance txs = do
  putStrLn $ "Current balance: " ++ show (getBalance txs)
