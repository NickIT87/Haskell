module Logic where

import Types

addTransaction :: Transaction -> [Transaction] -> [Transaction]
addTransaction tx txs = tx : txs

getBalance :: [Transaction] -> Double
getBalance = sum . map txAmount
  where
    txAmount (Transaction Income amt _ _ _) = amt
    txAmount (Transaction Expense amt _ _ _) = -amt
