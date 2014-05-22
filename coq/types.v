Inductive Nat : Set := Z : Nat | S : Nat -> Nat .

Fixpoint add (m n : Nat) : Nat :=
  match m with 
    | Z => n
    | S x => add x (S n)
  end.

Notation "A + B" := (add A B) (at level 50, left associativity). 

Inductive Fin : Nat -> Set := 
| F_z : forall n, Fin (S n)
| F_s : forall n,  Fin n -> Fin (S n)
.

Print Fin.
Print Fin_rec.
Print Nat_rec.

Inductive B : Set := TT : B | FF: B.

Inductive Tree : Set :=
| Empty : Tree
| Node : B -> Tree -> Tree -> Tree.

Inductive DTree : Nat -> Set := 
| DEmpty : DTree Z
| DNode : forall n m, B -> DTree n -> DTree m -> DTree (n + m).

Print Tree_rec.
Print DTree_rec.
Print DTree_rect.