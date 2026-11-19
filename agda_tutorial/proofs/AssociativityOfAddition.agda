-- Пример 4. Ассоциативность сложения
-- (a+b)+c=a+(b+c)
-- В этом примере cong показывает, что равенство можно переносить через функцию.
-- Если x = y, то f x = f y.

module AssociativityOfAddition where

open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Equality using (_≡_; refl)

-- Сложение задано рекурсией по второму аргументу:
-- прибавление нуля не меняет число, а прибавление suc n
-- означает «увеличить на один результат прибавления n».
_+_ : Nat → Nat → Nat
m + zero = m
m + suc n = suc (m + n)

-- Если x и y равны, то их образы под функцией f тоже равны.
-- Для конструктора равенства refl результат сразу сводится к refl.
cong : {A B : Set}
       {x y : A}
       (f : A → B)
       → x ≡ y
       → f x ≡ f y
cong f refl = refl

-- Доказываем ассоциативность сложения:
-- (a + b) + c равно a + (b + c).
+-assoc : (a b c : Nat)
        → (a + b) + c ≡ a + (b + c)
-- База индукции: при c = 0 обе стороны упрощаются до a + b.
+-assoc a b zero = refl
-- Индукционный шаг: для c = suc c обе стороны имеют внешний suc.
-- Применяем cong suc к предположению индукции для c.
+-assoc a b (suc c) = cong suc (+-assoc a b c)