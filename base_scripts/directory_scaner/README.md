# Сканировать текущую директорию полностью
runhaskell scan.hs .

# Найти только файлы
runhaskell scan.hs --files .

# Найти только директории
runhaskell scan.hs --dirs .

# Найти только файлы с расширением .hs
runhaskell scan.hs --ext=hs .

# Найти файлы с расширениями .hs и .php
runhaskell scan.hs --ext=hs,php .

# Ограничить глубину сканирования до 1 уровня
runhaskell scan.hs --max-depth=1 .

# Ограничить глубину сканирования до 2 уровней
runhaskell scan.hs --max-depth=2 .

# Ограничить глубину сканирования до 3 уровней
runhaskell scan.hs --max-depth=3 .

# Вывести только статистику без списка файлов и директорий
runhaskell scan.hs --count .

# Комбинация: только файлы с расширением .hs и статистика
runhaskell scan.hs --ext=hs --files --count .

# Комбинация: только директории до глубины 2
runhaskell scan.hs --dirs --max-depth=2 .

# Комбинация: файлы .hs и .php до глубины 3
runhaskell scan.hs --ext=hs,php --max-depth=3 .

# Комбинация: файлы и директории, вывод только статистики
runhaskell scan.hs --count .

# Комбинация всех флагов: файлы .hs, глубина 2, только статистика
runhaskell scan.hs --files --ext=hs --max-depth=2 --count .