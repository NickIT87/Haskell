module BasicInduction where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import Relation.Binary.PropositionalEquality.Properties using (module ≡-Reasoning)

open ≡-Reasoning

plus-zero-right : (n : ℕ) → n + zero ≡ n

plus-zero-right zero =
  begin
    zero + zero
  ≡⟨ refl ⟩
    zero
  ∎

plus-zero-right (suc n) =
  begin
    suc n + zero
  ≡⟨ refl ⟩
    suc (n + zero)
  ≡⟨ cong suc (plus-zero-right n) ⟩
    suc n
  ∎