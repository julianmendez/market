/-
directive scala
type Nat = Int
object Succ_ {
  def apply (n : Int) : Int = n + 1
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}
-/

notation "Succ_" => Nat.succ

/-
directive coq
Notation "'Succ_'" := S (at level 99).
-/
