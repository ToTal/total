module Nat where

open import Data.Unit
open import Data.Product

-- heterogeneous equality

infix 4 _≅_

data _≅_ {a} {A : Set a} (x : A) : ∀ {b} {B : Set b} → B → Set where
  refl : x ≅ x

-- An example with naturals

data nat : Set where 
  z : nat
  s : nat -> nat

-- Simulated eliminator
nat-elim : (x : nat) (P : nat -> Set) -> P z -> ((x : nat) -> P x -> P (s x)) -> P x
nat-elim z P pz fps = pz
nat-elim (s x) P pz fps = nat-elim x (λ z₁ → P (s z₁)) (fps z pz) (λ x₁ → fps (s x₁))

-- Case (the eliminator without inductive hypothesis
case-nat : (x : nat) (P : nat -> Set) -> P z -> ((x : nat) -> P (s x)) -> P x
case-nat x P pz fp = nat-elim x P pz (λ x₁ _ → fp x₁)

-- Below

Below-nat : (P : nat -> Set) -> nat -> Set
Below-nat P z = ⊤
Below-nat P (s n) = (Below-nat P n) × (P n)

below-nat : (P : nat -> Set) (p : (D : nat) -> Below-nat P D -> P D) -> (D : nat) -> Below-nat P D
below-nat P p z = tt
below-nat P p (s n) = (λ b → (b , p n b)) (below-nat P p n)

-- Rec

rec-nat : (P : nat -> Set) (p : (D : nat) -> Below-nat P D -> P D) -> (D : nat) -> P D
rec-nat P p D = p D (below-nat P p D)


-- no confusion : constructors are injective and disjoint

NoConfusion-nat : (P : Set) (x y : nat) -> Set
NoConfusion-nat P z z = P → P
NoConfusion-nat P (s n) (s m) = (n ≅ m → P) → P
NoConfusion-nat P z (s m) = P
NoConfusion-nat P (s n) z = P

noConfusion-nat : (P : Set) (x y : nat) (q : x ≅ y) -> NoConfusion-nat P x y
noConfusion-nat P z z refl = λ p → p
noConfusion-nat P (s x) .(s x) refl = λ p → p refl

-- noCycle : disproves any cyclic equation

