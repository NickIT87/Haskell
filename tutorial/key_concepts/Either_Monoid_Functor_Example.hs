module Either_Monoid_Functor_Example where

import Data.Char (toLower, isDigit)
import Data.List (stripPrefix, isInfixOf)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

-- Допустим, у нас есть "грязные" данные из формы регистрации
rawInput :: [(String, String)]
rawInput =
  [ ("name", "John Doe")
  , ("age", "30")
  , ("email", "john@example.com")
  , ("subscribe", "yes")
  , ("extra", "foo") -- Какое-то лишнее поле, которого нет в нашей модели
  ]

-- Наша чистая модель данных с использованием типажа Semigroup/Monoid для имени
newtype Name = Name { unName :: String }
  deriving (Show, Eq)

-- Реализуем Semigroup для объединения имен (например, если имя придет из двух полей)
instance Semigroup Name where
  Name a <> Name b = Name (a ++ " " ++ b)

instance Monoid Name where
  mempty = Name ""

data User = User
  { name  :: Name
  , age   :: Int
  , email :: String
  , isSubscribed :: Bool
  } deriving (Show)

-- Функция для безопасного преобразования строки в Bool с учетом разных вариантов ввода
parseBool :: String -> Maybe Bool
parseBool s = case map toLower s of
  "yes"   -> Just True
  "y"     -> Just True
  "true"  -> Just True
  "1"     -> Just True
  "no"    -> Just False
  "n"     -> Just False
  "false" -> Just False
  "0"     -> Just False
  _       -> Nothing

-- Валидация email (примитивная, просто проверяем наличие '@' и '.')
validateEmail :: String -> Either String String
validateEmail e
  | '@' `notElem` e = Left "Email must contain '@'"
  | '.' `notElem` e = Left "Email must contain a dot"
  | otherwise       = Right e

-- Основная функция парсинга с использованием аппликативного функтора (Either)
-- Здесь мы объединяем несколько валидаций в одну
parseUser :: [(String, String)] -> Either String User
parseUser input = do
  -- Превращаем список в Map-подобное поведение для удобства
  let lookup' key = case lookup key input of
        Nothing -> Left $ "Missing field: " ++ key
        Just v  -> Right v

  -- Используем do-нотацию для Either (монада!)
  nameStr    <- lookup' "name"
  ageStr     <- lookup' "age"
  emailStr   <- lookup' "email"
  subStr     <- lookup' "subscribe"

  -- Парсим возраст с помощью readMaybe из Text.Read
  ageInt     <- case readMaybe ageStr of
                  Nothing -> Left $ "Invalid age: " ++ ageStr
                  Just a  -> Right a

  -- Парсим подписку
  subscribed <- case parseBool subStr of
                  Nothing -> Left $ "Invalid subscribe value: " ++ subStr
                  Just b  -> Right b

  -- Валидируем email
  validEmail <- validateEmail emailStr

  -- Все ок, собираем пользователя
  return $ User (Name nameStr) ageInt validEmail subscribed

-- Функция, демонстрирующая mapMaybe (комбинация map и filter)
-- Извлекаем только те поля, которые соответствуют определенному критерию
extractPrefixedFields :: String -> [(String, String)] -> [String]
extractPrefixedFields prefix = mapMaybe extractValue
  where
    extractValue (key, value) = case stripPrefix prefix key of
      Just _  -> Just value
      Nothing -> Nothing

-- Пример использования filter и композиции
filterValidNames :: [User] -> [Name]
filterValidNames = map name . filter (not . null . unName . name)

main :: IO ()
main = do
  putStrLn "=== Парсинг пользователя ==="
  case parseUser rawInput of
    Left err -> putStrLn $ "Error: " ++ err
    Right user -> do
      print user
      -- Проверяем моноид (Semigroup) для Name
      let newName = Name "Mr." <> name user <> Name "III"
      putStrLn $ "Updated name with monoid: " ++ unName newName

  putStrLn "\n=== Извлечение префиксов (mapMaybe) ==="
  let extraFields = extractPrefixedFields "extra_" [("extra_foo", "bar"), ("extra_baz", "qux"), ("normal", "val")]
  print extraFields

  putStrLn "\n=== Работа с Maybe как с Functor ==="
  let maybeAge = lookup "age" rawInput >>= readMaybe :: Maybe Int
  -- fmap над Maybe
  let doubledAge = fmap (*2) maybeAge
  print $ "Double age: " ++ show doubledAge

  -- Демонстрация Applicative стиля (хотя в do это не так заметно)
  putStrLn "\n=== Applicative style (пример) ==="
  let a = Just 5
      b = Just 10
  -- Поднимаем функцию (+) в контекст Maybe
  print $ (+) <$> a <*> b  -- Just 15


{- ####################################################################
   РАЗБОР ВЫВОДА ПРОГРАММЫ
   ####################################################################

❯ runhaskell Either_Monoid_Functor_Example.hs

=== Парсинг пользователя ===
User {name = Name {unName = "John Doe"}, age = 30, email = "john@example.com", isSubscribed = True}

🔍 ЧТО ПРОИСХОДИТ:
   rawInput = [("name","John Doe"), ("age","30"), ("email","john@example.com"), 
               ("subscribe","yes"), ("extra","foo")]
   
   1. lookup' "name"      → Right "John Doe"
   2. lookup' "age"       → Right "30"      → readMaybe → Just 30  → Right 30
   3. lookup' "email"     → Right "john@example.com" → validateEmail → OK
   4. lookup' "subscribe" → Right "yes"     → parseBool → Just True → Right True
   5. Все поля успешно спарсены, создаём User
   
   Поле "extra" игнорируется, так как мы запрашиваем только нужные ключи.

=== Обновление имени с моноидом ===
Updated name with monoid: Mr. John Doe III

🔍 ЧТО ПРОИСХОДИТ:
   Instance Semigroup Name: Name a <> Name b = Name (a ++ " " ++ b)
   
   Name "Mr." <> Name "John Doe" <> Name "III"
   ↓
   1. Name "Mr." <> Name "John Doe" = Name "Mr. John Doe"
   2. Name "Mr. John Doe" <> Name "III" = Name "Mr. John Doe III"
   
   Левоассоциативная операция: ((Mr. <> John) <> III)

=== Извлечение префиксов (mapMaybe) ===
["bar","qux"]

🔍 ЧТО ПРОИСХОДИТ:
   Вход: [("extra_foo","bar"), ("extra_baz","qux"), ("normal","val")]
   Префикс: "extra_"
   
   mapMaybe применяет функцию и отбрасывает Nothing:
   • ("extra_foo","bar") → stripPrefix "extra_" "extra_foo" = Just "foo" → Just "bar"
   • ("extra_baz","qux") → stripPrefix "extra_" "extra_baz" = Just "baz" → Just "qux"  
   • ("normal","val")    → stripPrefix "extra_" "normal"    = Nothing    → Nothing
   
   Результат: [Just "bar", Just "qux", Nothing] → ["bar", "qux"]

=== Работа с Maybe как с Functor ===
"Double age: Just 60"

🔍 ЧТО ПРОИСХОДИТ:
   let maybeAge = lookup "age" rawInput >>= readMaybe :: Maybe Int  -- Just 30
   let doubledAge = fmap (*2) maybeAge
   
   fmap (или <$>) применяет функцию к значению внутри контекста, не распаковывая его:
   Just 30
      ↓  (*2)
   Just 60
   
   Если бы было Nothing: fmap (*2) Nothing = Nothing

=== Applicative style (пример) ===
Just 15

🔍 ЧТО ПРОИСХОДИТ:
   let a = Just 5
       b = Just 10
   print $ (+) <$> a <*> b
   
   По шагам:
   1. (+) <$> Just 5  = Just (5+)    -- функция (5+) внутри Just
   2. Just (5+) <*> Just 10 = Just (5 + 10) = Just 15
   
   Если один из аргументов Nothing: (+) <$> Nothing <*> Just 10 = Nothing

═══════════════════════════════════════════════════════════════════════
   КЛЮЧЕВЫЕ КОНЦЕПЦИИ В ДЕЙСТВИИ:

   📌 Monad (Either)  - последовательные операции с ранним выходом при ошибке
   📌 Semigroup/Monoid - объединение значений (Name) с нейтральным элементом
   📌 Functor         - применение функции к значению в контексте (fmap)
   📌 Applicative     - применение многоместной функции к значениям в контексте
   📌 mapMaybe        - комбинация map и filter (трансформация + отсев Nothing)
   
   Все операции безопасны - контекст (Maybe, Either) автоматически обрабатывает
   отсутствие значений и ошибки!
   ####################################################################
-}