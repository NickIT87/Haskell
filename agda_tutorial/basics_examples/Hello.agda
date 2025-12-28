module Hello where

-- Определяем натуральные числа
data Nat : Set where
  zero : Nat
  suc  : Nat -> Nat

-- Сложение
_+_ : Nat -> Nat -> Nat
zero  + n = n
suc m + n = suc (m + n)

-- Пример вычисления
two : Nat
two = suc (suc zero)

three : Nat
three = suc two

five : Nat
five = two + three
