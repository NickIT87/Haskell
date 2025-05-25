{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text)
import qualified Data.Text.IO as T

-- Определяем сущность User
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    age Int
    deriving Show
|]

main :: IO ()
main = runSqlite "users.db" $ do
    runMigration migrateAll

    -- CREATE
    liftIO $ putStrLn "Добавление пользователя..."
    userId <- insert $ User "Alice" 30

    -- READ
    liftIO $ putStrLn "Чтение пользователя..."
    maybeUser <- get userId
    liftIO $ print maybeUser

    -- UPDATE
    liftIO $ putStrLn "Обновление возраста..."
    update userId [UserAge =. 31]

    -- READ снова
    liftIO $ putStrLn "Пользователь после обновления:"
    updatedUser <- get userId
    liftIO $ print updatedUser

    -- DELETE
    liftIO $ putStrLn "Удаление пользователя..."
    delete userId

    -- Проверка удаления
    deletedUser <- get userId
    liftIO $ putStrLn "После удаления:"
    liftIO $ print deletedUser
