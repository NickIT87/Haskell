module Ops5 where

open import Data.List using (List; []; _∷_; length; take; drop; reverse; _++_)
open import Data.Char using (Char; _==_)
open import Data.Nat using (ℕ; _+_)
open import Data.Nat.DivMod using (_div_)
open import Data.Bool using (Bool; true; false; if_then_else_)

------------------------------------------------------------------------
-- startsWith : проверка, что первый список — префикс второго

startsWith : List Char → List Char → Bool
startsWith [] _ = true
startsWith (_ ∷ _) [] = false
startsWith (x ∷ xs) (y ∷ ys) =
  if x == y then startsWith xs ys else false

------------------------------------------------------------------------
-- replacePrefix : заменить префикс old на new (один раз)

replacePrefix : List Char → List Char → List Char → List Char
replacePrefix old new word =
  if startsWith old word
  then new ++ drop (length old) word
  else word

------------------------------------------------------------------------
-- Половины строки

leftLen : List Char → ℕ
leftLen c = (length c + 1) div 2

rightLen : List Char → ℕ
rightLen c = leftLen c + 1

------------------------------------------------------------------------
-- Основная функция (цикл = рекурсия)

ops5 : List Char → List (List Char) → List Char
ops5 word [] = word
ops5 word (c ∷ cs) =
  let
    w-left-1  = take ((length c div 2) + 1) (reverse c)
    w-left-2  = take (leftLen c) c

    w-right-1 = take (rightLen c) c
    w-right-2 = take (length c div 2) (reverse c)
  in
  if startsWith w-left-1 word then
    replacePrefix w-left-1 w-left-2 word
  else if startsWith w-right-1 word then
    replacePrefix w-right-1 w-right-2 word
  else
    ops5 word cs
