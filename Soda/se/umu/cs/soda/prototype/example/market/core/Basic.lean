/-
directive scala
object Succ_ {
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}
-/

notation "Succ_" => Nat.succ
notation "Int" => Nat

/-
directive coq
Notation "head '+:' tail" := (cons (head) (tail) ) (at level 99).
Notation "'Succ_'" := S (at level 99).
Notation "'Int'" := nat (at level 99).
-/

notation "Nat" => Int
