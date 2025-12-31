module Example where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Vec using (Vec; []; _∷_)
open import Data.Bool using (Bool; true)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

y : Bool
y = true

x : ℕ
x = 42

v : Vec ℕ 3
v = 1 ∷ 2 ∷ 3 ∷ []

sumVec : {n : ℕ} → Vec ℕ n → ℕ
sumVec [] = 0
sumVec (x ∷ xs) = x + sumVec xs

total : ℕ
total = sumVec v

sumZeroRight : (n : ℕ) → n + 0 ≡ n
sumZeroRight zero    = refl
sumZeroRight (suc n) = cong suc (sumZeroRight n)
