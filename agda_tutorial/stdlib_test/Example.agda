module Example where

-- Подключаем стандартную библиотеку
open import Data.Nat
open import Data.Vec
open import Data.Bool
open import Relation.Binary.PropositionalEquality

-- Пример Bool
y : Bool
y = true

-- Пример Nat
x : Nat
x = 42  -- работает, так как используем Data.Nat

-- Пример зависимого типа: Vec
v : Vec Nat 3
v = 1 ∷ 2 ∷ 3 ∷ []

-- Функция на Vec
sumVec : {n : Nat} -> Vec Nat n -> Nat
sumVec [] = 0
sumVec (x ∷ xs) = x + sumVec xs

total : Nat
total = sumVec v

-- Пример доказательства: сумма с нулём
sumZeroRight : (n : Nat) -> n + 0 ≡ n
sumZeroRight zero = refl
sumZeroRight (suc n) = cong suc (sumZeroRight n)
