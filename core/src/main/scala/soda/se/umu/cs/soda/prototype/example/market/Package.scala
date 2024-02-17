package soda.se.umu.cs.soda.prototype.example.market

/*
 * This package contains classes to model a market
 *
 */

object Succ_ {
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}

/*
directive lean
notation "Succ_" => Nat.succ
notation "Int" => Nat
*/

/*
directive coq
Notation "head '+:' tail" := (cons (head) (tail) ) (at level 99) .
Notation "'Succ_'" := S (at level 99) .
Notation "'Int'" := nat (at level 99) .
*/

type Nat = Int


type Money = Int

trait Item
{

  def   owner : Nat
  def   price : Money
  def   advertised : Boolean

}

case class Item_ (owner : Nat, price : Money, advertised : Boolean) extends Item

object Item {
  def mk (owner : Nat) (price : Money) (advertised : Boolean) : Item =
    Item_ (owner, price, advertised)
}

trait Market
{

  def   accounts : List [Money]
  def   items : List [Item]

}

case class Market_ (accounts : List [Money], items : List [Item]) extends Market

object Market {
  def mk (accounts : List [Money]) (items : List [Item]) : Market =
    Market_ (accounts, items)
}

trait MarketMod
{

  def   bit : Boolean

  private lazy val _mm : MyList = MyList_ (true)

/*
  directive lean
  notation "_mm.get" => MyList.get
  notation "_mm.set" => MyList.set
  notation "_mm.foldl" => MyList.foldl
*/

  def as_market (market : Market) : Market =
    Market.mk (market .accounts) (market .items)

  def get_items (market : Market) : List [Item] =
    market match  {
      case Market_ (_accounts, items) => items
    }

/*
  directive lean
  theorem
    proj_items (market : Market) (accounts : List (Money)) (items : List (Item))
      : (market = Market_ (accounts, items) ) -> get_items (market) = items := by
    rewrite [get_items]
    cases market
    intro h1
    rewrite [h1]
    simp
*/

  private def _advertise (items : List [Item] ) (item_id : Nat) : List [Item] =
    (_mm .get [Item] (items) (item_id) ) match  {
      case Some (item) =>
        _mm .set [Item] (items) (item_id) (Item_ (item .owner, item .price, true) )
      case None => items
    }

  def advertise (market : Market) (item_id : Nat) : Market =
    Market.mk (market .accounts) (_advertise (market .items) (item_id) )

  private def _remove_ad (items : List [Item] ) (item_id : Nat) : List [Item] =
    (_mm .get [Item] (items) (item_id) ) match  {
      case Some (item) =>
        _mm .set [Item] (items) (item_id) (Item_ (item .owner, item .price, false) )
      case None => items
    }

  def remove_ad (market : Market) (item_id : Nat) : Market =
    Market.mk (market .accounts) (_remove_ad (market .items) (item_id) )

  private def _transfer_with_balances (accounts : List [Money] ) (origin : Nat) (target : Nat)
      (amount : Money) (origin_balance : Money) (target_balance : Money) : List [Money] =
    _mm .set [Money] (_mm .set [Money] (accounts)
      (origin) (origin_balance - amount) ) (target) (target_balance + amount)

  private def _transfer_with (accounts : List [Money] ) (origin : Nat) (target : Nat) (amount : Money)
      (origin_balance : Money) : List [Money] =
    (_mm .get [Money] (accounts) (target) ) match  {
      case Some (target_balance) =>
        _transfer_with_balances (accounts) (origin) (target)
          (amount) (origin_balance) (target_balance)
      case None => accounts
    }

  private def _transfer (accounts : List [Money] ) (origin : Nat) (target : Nat) (amount : Money)
      : List [Money] =
    (_mm .get [Money] (accounts) (origin) ) match  {
      case Some (origin_balance) =>
        _transfer_with (accounts) (origin) (target) (amount) (origin_balance)
      case None => accounts
    }

  def sell (market : Market) (item_id : Nat) (buyer : Nat) : Market =
    (_mm .get [Item] (market .items) (item_id) ) match  {
      case Some (item) =>
        Market .mk (
          _transfer (market .accounts) (buyer) (item .owner) (item .price) ) (
          _mm .set [Item] (market .items) (item_id) (Item_ (buyer, item .price, false) )
        )
      case None => market
    }

  private def _sum_pair (a : Money) (b : Money) : Money =
    a + b

  def assets (market : Market) : Money =
    _mm .foldl [Money, Money] (market .accounts) (0) (_sum_pair)

}

case class MarketMod_ (bit : Boolean) extends MarketMod

object MarketMod {
  def mk (bit : Boolean) : MarketMod =
    MarketMod_ (bit)
}




trait IndexOption [A ]
{

  def   current_index : Nat
  def   maybe_elem : Option [A]

}

case class IndexOption_ [A] (current_index : Nat, maybe_elem : Option [A]) extends IndexOption [A]

object IndexOption {
  def mk [A] (current_index : Nat) (maybe_elem : Option [A]) : IndexOption [A] =
    IndexOption_ [A] (current_index, maybe_elem)
}

trait ChangeWindow [A ]
{

  def   current_index : Nat
  def   target_index : Nat
  def   new_value : A
  def   rev_accum : List [A]

}

case class ChangeWindow_ [A] (current_index : Nat, target_index : Nat, new_value : A, rev_accum : List [A]) extends ChangeWindow [A]

object ChangeWindow {
  def mk [A] (current_index : Nat) (target_index : Nat) (new_value : A) (rev_accum : List [A]) : ChangeWindow [A] =
    ChangeWindow_ [A] (current_index, target_index, new_value, rev_accum)
}

trait MyList
{

  def   bit : Boolean

/*
 * `_tailrec_foldl` is a 'fold left' function for parameterized types.
 * This definition of fold left is tail recursive.
 */

  private def _tailrec_foldl [A , B ] (list : List [A] ) (current : B)
      (next : B => A => B) : B =
    list match  {
      case Nil => current
      case (head) :: (tail) =>
        _tailrec_foldl [A, B] (tail) (next (current) (head) ) (next)
    }

/*
 * `foldl` is a 'fold left' function for parameterized types that uses `_tailrec_foldl`.
  */

  def foldl [A , B ] (list : List [A] ) (initial : B)
      (next : B => A => B) : B =
    _tailrec_foldl [A, B] (list) (initial) (next)

/*
 * `length` defined using fold left.
 * This uses foldl, which is tail recursive.
 */

  def length_fl [A ] (list : List [A] ) : Nat =
    _tailrec_foldl [A, Nat] (list) (0) (
       (accum : Nat) =>
         (_elem : A) => accum + 1
    )

/*
  directive lean
  theorem
    len_fl_accum (A : Type) (list : List (A) )
       : forall (accum : Nat) ,
        _tailrec_foldl (A) (Nat) (list) (accum) (fun (accum : Nat) => fun (elem : A) => accum + 1) =
           _tailrec_foldl (A) (Nat) (list) (0) (fun (accum : Nat) => fun (elem : A) => accum + 1) + accum := by
      induction list with
      | nil =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl, Nat.zero_add]
        rfl
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl]
        rewrite [ih (1)]
        rewrite [ih (n + 1)]
        rewrite [Nat.add_assoc]
        rewrite [Nat.add_comm 1]
        rfl
*/

  private def _tailrec_length [A ] (list : List [A] ) (accum : Nat) : Nat =
    list match  {
      case Nil => accum
      case (_head) :: (tail) =>
        _tailrec_length [A] (tail) (accum + 1)
    }

/*
  directive lean
  theorem
    len_tr_accum (A : Type) (list : List (A) )
      : forall (accum : Nat) ,
        _tailrec_length (A) (list) (accum)  = _tailrec_length (A) (list) (0) + accum := by
      induction list with
      | nil =>
        intro n
        rewrite [_tailrec_length, _tailrec_length, Nat.zero_add]
        rfl
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_length, _tailrec_length]
        rewrite [ih (1)]
        rewrite [ih (n + 1)]
        rewrite [Nat.add_assoc]
        rewrite [Nat.add_comm 1]
        rfl
*/

  def length_tr [A ] (list : List [A] ) : Nat =
    _tailrec_length [A] (list) (0)

  def length_def [A ] (list : List [A] ) : Nat =
    list match  {
      case Nil => 0
      case (_head) :: (tail) => length_def [A] (tail) + 1
    }

/*
  directive lean
  theorem
  len_fl_eq_len_def (A : Type) (list : List (A))
      : length_fl (A) (list) = length_def (A) (list) := by
    rewrite [length_fl]
    induction list with
    | nil =>
      rewrite [_tailrec_foldl, length_def]
      rfl
    | cons head tail ih =>
      rewrite [_tailrec_foldl, len_fl_accum]
      rewrite [ih]
      rewrite [length_def]
      rfl
*/

/*
  directive lean
  theorem
    len_tr_eq_len_def
      : length_tr = length_def := by
    funext A list
    rewrite [length_tr]
    induction list with
    | nil =>
      rewrite [_tailrec_length, length_def]
      rfl
    | cons head tail ih =>
      rewrite [_tailrec_length, len_tr_accum]
      rewrite [ih]
      rewrite [length_def]
      rfl
*/

  def length [A ] (list : List [A] ) : Nat =
    length_fl [A] (list)

/*
 * reverse
 */

  private def _tailrec_reverse [A ] (list : List [A] ) (accum : List [A] ) : List [A] =
    list match  {
      case Nil => accum
      case (head) :: (tail) => _tailrec_reverse [A] (tail) ( (head) :: (accum) )
    }

  def reverse_tr [A ] (list : List [A] ) : List [A] =
    _tailrec_reverse [A] (list) (Nil)

  def reverse_fl [A ] (list : List [A] ) : List [A] =
    _tailrec_foldl [A, List [A] ] (list) (Nil) (
       (accum : List [A] ) =>
         (elem : A) =>
          (elem) :: (accum)
    )

/*
  directive lean
  theorem
    rev_fl_accum (A : Type) (list : List (A))
      : forall (current: List (A) ),
        _tailrec_foldl (A) (List (A) ) (list) (current)
          (fun (accum : List (A) ) =>
            fun (elem : A) =>
               (elem) :: (accum)
          ) = _tailrec_reverse (A) (list) (current) := by
      induction list with
      | nil =>
        intro other
        rewrite [_tailrec_foldl, _tailrec_reverse]
        rfl
      | cons head tail ih =>
        intro other
        rewrite [_tailrec_foldl, _tailrec_reverse]
        rewrite [ih ((head) :: (other))]
        rfl
*/

/*
  directive lean
  theorem
    rev_tr_eq_rev_fl
      (A : Type) (list : List (A) )
        : reverse_fl (A) (list) = reverse_tr (A) (list) := by
    rewrite [reverse_fl, reverse_tr, rev_fl_accum]
    rfl
*/

/*
  directive lean
  theorem
    len_rev_accum (A : Type) (list : List (A))
      : forall (accum : List (A) ),
        length_def (A) (_tailrec_reverse (A) (list) (accum)) =
            length_def (A) (_tailrec_reverse (A) (list) ([])) + length_def (A) (accum) := by
      induction list with
      | nil =>
        intro other
        rewrite [_tailrec_reverse, _tailrec_reverse, length_def, Nat.zero_add]
        rfl
      | cons head tail ih =>
        intro other
        rewrite [_tailrec_reverse, _tailrec_reverse]
        rewrite [ih ((head) :: ([]))]
        rewrite [ih ((head) :: (other))]
        rewrite [length_def, length_def, length_def]
        rewrite [Nat.add_assoc, Nat.add_comm 1]
        rfl
*/

  def reverse [A ] (list : List [A] ) : List [A] =
    reverse_fl [A] (list)

/*
 * map
 */

  def map_fl [A , B ] (list : List [A] ) (f : A => B) : List [B] =
    reverse_fl [B] (
      foldl [A, List [B] ] (list) (Nil) (
         (accum : List [B] ) =>
           (elem : A) =>
            (f (elem) ) :: (accum)
      )
    )

  def map_def [A , B ] (list : List [A] ) (func : A => B ) : List [B] =
    list match  {
      case Nil => Nil
      case (head) :: (tail) => (func (head) ) :: (map_def [A, B] (tail) (func) )
    }

  def map [A , B ] (list : List [A] ) (f : A => B) : List [B] =
    map_fl [A, B] (list) (f)

/*
 * concat
 */

  def concat [A ] (first : List [A] ) (second : List [A] ) : List [A] =
    _tailrec_foldl [A, List [A] ] (reverse_fl [A] (first) ) (second) (
       (accum : List [A] ) =>
         (elem : A) =>
          (elem) :: (accum)
    )

  def monus1 (index : Nat) : Nat =
    index match  {
      case Succ_ (k) => k
      case _otherwise  => 0
    }

/*
  directive lean
  theorem
    monus1_succ
      : forall (index : Nat),
        monus1 (Nat.succ (index)) = index := by
    intro idx
    rewrite [monus1]
    simp
*/

  def get [A ] (list : List [A] ) (index : Nat) : Option [A] =
    (foldl [A, IndexOption [A] ] (list) (
      IndexOption .mk (0) (None) ) (
         (accum : IndexOption [A] ) =>
           (elem : A) =>
            if ( (accum .current_index == index)
            ) IndexOption .mk (accum .current_index + 1) (Some (elem) )
            else IndexOption .mk (accum .current_index + 1) (accum .maybe_elem)
      )
    ) .maybe_elem

  private def _replace_if_in_place [A ] (current_index : Nat) (target_index : Nat) (old_value : A) (
      new_value : A) : A =
    if ( (current_index == target_index)
    ) new_value
    else old_value

  private def _apply_replacement [A ] (p : ChangeWindow [A] ) (element : A) : ChangeWindow [A] =
    ChangeWindow .mk (p .current_index + 1) (p .target_index) (p .new_value) (
      (_replace_if_in_place [A] (p .current_index) (p .target_index) (element) (p .new_value)
      ) :: p .rev_accum
    )

  private def _initial_window [A ] (index : Nat) (new_value : A) : ChangeWindow [A] =
    ChangeWindow .mk (0) (index) (new_value) (Nil)

  def set_fl [A ] (list : List [A] ) (index : Nat) (new_value : A) : List [A] =
    reverse_fl [A] (
      (foldl [A, ChangeWindow [A] ] (list) (_initial_window [A] (index) (new_value) ) (
        _apply_replacement [A] ) ) .rev_accum
    )

  def set_def [A ] (list : List [A] ) (index : Nat) (element : A) : List [A] =
    list match  {
      case Nil => Nil
      case (_head) :: (tail) =>
        if ( index == 0
        ) (element) :: (tail)
        else (element) :: (set_def [A] (tail) (monus1 (index) ) (element) )
    }

/*
  directive lean
  theorem    len_set (A : Type) (list : List (A)) (element : A)
      : forall (index : Nat),
        length_def (A) (set_def (A) (list) (index) (element) ) = length_def (A) (list) := by
    induction list with
    | nil =>
      intro idx
      rewrite [set_def, length_def]
      rfl
    | cons head tail ih =>
       intro idx
       rewrite [set_def, length_def]
       cases idx with
       | zero =>
         rewrite [monus1]
         rewrite [Nat.zero_eq]
         rfl
       | succ k =>
         rewrite [monus1]
         simp
         rewrite [length_def]
         rewrite [ih]
         rfl
*/

  def set [A ] (list : List [A] ) (index : Nat) (element : A) : List [A] =
    set_fl [A] (list) (index) (element)

}

case class MyList_ (bit : Boolean) extends MyList

object MyList {
  def mk (bit : Boolean) : MyList =
    MyList_ (bit)
}

