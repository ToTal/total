module Notes where

data nat : Set where
  z : nat
  s : nat -> nat



data fin : nat -> Set where
  fz : (n : nat) -> fin (s n)
  fs : (n : nat) -> fin n -> fin (s n)


data leq : (n : nat) -> fin n -> fin n -> Set where
  leqz : (n : nat) (j : fin (s n)) -> leq (s n) (fz n) j
  leqs : (n : nat) (i : fin n) (j : fin n) (p : leq n i j) -> leq (s n) (fs n i) (fs n j)


trans : (n : nat) (i : fin n) (j : fin n) (k : fin n) (p : leq n i j) (q : leq n j k) -> leq n i k
trans .(s n) .(fz n) j k (leqz n .j) q = leqz n k
trans .(s n) .(fs n i) .(fs n j) .(fs n j₁) (leqs n i j p) (leqs .n .j j₁ q) = leqs n i j₁ (trans n i j j₁ p q)









