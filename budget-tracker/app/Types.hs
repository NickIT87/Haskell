{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Time (Day)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data TransactionType = Income | Expense
  deriving (Show, Read, Eq, Generic)

instance FromJSON TransactionType
instance ToJSON TransactionType

data Transaction = Transaction
  { tType     :: TransactionType
  , amount    :: Double
  , category  :: String
  , date      :: Day
  , comment   :: String
  } deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON Transaction
