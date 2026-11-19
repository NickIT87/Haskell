module Example where

open import Data.Nat using (ℕ; _+_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Binary.PropositionalEquality.Properties using (module ≡-Reasoning)

open ≡-Reasoning

proof : 1 + 1 ≡ 2
proof =
  begin
    1 + 1
  ≡⟨ refl ⟩
    2
  ∎