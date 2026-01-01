-- Доказательство теоремы: ∀ n : ℕ → n + 0 ≡ n

-- Объявляем модуль с именем InductionProof.
-- Имя модуля должно совпадать с именем файла: InductionProof.agda
module InductionProof where


-- Импортируем стандартный модуль с натуральными числами.
-- using (...) означает, что мы явно указываем,
-- какие имена хотим взять из модуля (хорошая практика).
open import Data.Nat using
  ( ℕ      -- Тип натуральных чисел (0, 1, 2, 3, ...)
  ; zero   -- Конструктор нуля: zero : ℕ
  ; suc    -- Конструктор следующего числа: suc : ℕ → ℕ
  ; _+_    -- Операция сложения: _+_ : ℕ → ℕ → ℕ
  )


-- Импортируем пропозициональное равенство.
-- Это равенство используется в доказательствах.
open import Relation.Binary.PropositionalEquality using
  ( _≡_    -- Тип равенства: x ≡ y означает "x равно y"
  ; refl   -- Доказательство рефлексивности: x ≡ x
  ; cong   -- Если x ≡ y, то f x ≡ f y
  )


-- Формулируем теорему:
-- для любого натурального числа n выполняется n + 0 ≡ n
--
-- Важно:
-- • это ТИП
-- • значение этого типа и есть доказательство
-- • стрелка (→) читается как "для любого n"
plus-zero-right : (n : ℕ) → n + zero ≡ n


-- БАЗА ИНДУКЦИИ
--
-- Рассматриваем случай n = zero
-- Цель, которую видит Agda:
--
--   zero + zero ≡ zero
--
-- После раскрытия определения сложения:
--
--   zero ≡ zero
--
-- Это тривиально истинно, поэтому используем refl
plus-zero-right zero =
  refl


-- ШАГ ИНДУКЦИИ
--
-- Предполагаем, что утверждение уже доказано для n:
--
--   plus-zero-right n : n + zero ≡ n
--
-- Нужно доказать его для suc n
--
-- Цель:
--
--   suc n + zero ≡ suc n
--
-- После упрощения сложения:
--
--   suc (n + zero) ≡ suc n
--
-- Мы берём доказательство:
--
--   n + zero ≡ n
--
-- и применяем к обеим сторонам функцию suc,
-- используя лемму cong
plus-zero-right (suc n) =
  cong suc (plus-zero-right n)

-- □ ← здесь доказательство закончено 

-- тот же код без комментариев:

-- module InductionProof where
-- open import Data.Nat using (ℕ; zero; suc; _+_)
-- open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
-- plus-zero-right : (n : ℕ) → n + zero ≡ n
-- plus-zero-right zero =
--   refl
-- plus-zero-right (suc n) =
--   cong suc (plus-zero-right n)