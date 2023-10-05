{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple

data Person = Person { name :: String, age :: Int } deriving Show

instance FromRow Person where
    fromRow = Person <$> field <*> field

instance ToRow Person where
    toRow (Person name age) = toRow (name, age)

createTable :: Connection -> IO ()
createTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS person (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age INTEGER)"

insertPerson :: Connection -> Person -> IO ()
insertPerson conn (Person name age) = execute conn "INSERT INTO person (name, age) VALUES (?, ?)" (name, age)

getPersonById :: Connection -> Int -> IO (Maybe Person)
getPersonById conn id = do
    result <- query conn "SELECT * FROM person WHERE id = ?" (Only id)
    case result of
        [person] -> return $ Just person
        _        -> return Nothing

updatePerson :: Connection -> Int -> Person -> IO ()
updatePerson conn id (Person name age) = execute conn "UPDATE person SET name = ?, age = ? WHERE id = ?" (name, age, id)

deletePerson :: Connection -> Int -> IO ()
deletePerson conn id = execute conn "DELETE FROM person WHERE id = ?" (Only id)

main :: IO ()
main = do
    conn <- open "test.db"
    createTable conn

    -- Create
    insertPerson conn (Person "John Doe" 30)

    -- Read
    maybePerson <- getPersonById conn 1
    print maybePerson

    -- Update
    updatePerson conn 1 (Person "John Smith" 30)

    -- Read updated person
    maybeUpdatedPerson <- getPersonById conn 1
    print maybeUpdatedPerson

    -- Delete
    deletePerson conn 1

    -- Read deleted person
    maybeDeletedPerson <- getPersonById conn 1
    print maybeDeletedPerson

    close conn
