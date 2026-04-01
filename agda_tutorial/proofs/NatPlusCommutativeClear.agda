module NatPlusCommutativeClear where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; sym; trans)

+-suc : (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n = refl
+-suc (suc m) n = cong suc (+-suc m n)

+-identityʳ : (m : ℕ) → m + zero ≡ m
+-identityʳ zero = refl
+-identityʳ (suc m) = cong suc (+-identityʳ m)

+-comm : (m n : ℕ) → m + n ≡ n + m
+-comm zero n = sym (+-identityʳ n)
+-comm (suc m) n =
  trans
    (cong suc (+-comm m n))
    (sym (+-suc n m))

