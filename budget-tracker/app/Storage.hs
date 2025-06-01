module Storage where

import Types
import Data.Aeson (decodeFileStrict', encodeFile)
import Data.Maybe (fromMaybe)

type FilePathDB = FilePath

loadTransactions :: FilePathDB -> IO [Transaction]
loadTransactions path = do
  maybeData <- decodeFileStrict' path
  return $ fromMaybe [] maybeData

saveTransactions :: FilePathDB -> [Transaction] -> IO ()
saveTransactions path txs = encodeFile path txs
