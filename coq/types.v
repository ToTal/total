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
Print Fin_rect.
Print Nat_rect.

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

Definition plus : Nat -> Nat -> Nat.
intros m n.
refine (Nat_rec _ n _ m).
intros ms ns.
exact (S ns).
Defined.

Definition plus' := 
  fun m n : Nat => 
    Nat_rec (fun _ : Nat => Nat) n (fun _ ns : Nat => S ns) m.

Eval compute in (plus Z Z).
Eval compute in (plus Z (S Z)).
Eval compute in (plus (S Z) Z).
Eval compute in (plus (S Z) (S Z)).

Eval compute in (plus' Z Z).
Eval compute in (plus' Z (S Z)).
Eval compute in (plus' (S Z) Z).
Eval compute in (plus' (S Z) (S Z)).

