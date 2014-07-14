Inductive eq {A : Set} (x : A) : A -> Set :=
| refl : eq x x.

Inductive JMeq {A : Set} (x : A) : forall {B : Set} (y : B), Set :=
| jrefl : JMeq x x.

Lemma uip_refl (T : Set) (n : T) (pf : eq n n) : JMeq pf (refl n).
Print eq_rect.
eapply (eq_rect T n (fun y p => JMeq p (refl n))).
eapply jrefl.
Defined.
Print uip_refl.