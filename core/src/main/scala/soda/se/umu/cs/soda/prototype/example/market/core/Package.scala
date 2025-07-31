package soda.se.umu.cs.soda.prototype.example.market.core

/*
 * This package contains classes to model a market
 *
 */





type Nat = Int
object Succ_ {
  def apply (n : Int) : Int = n + 1
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}

/*
directive lean
notation "Succ_" => Nat.succ
*/

/*
directive coq
Notation "'Succ_'" := S (at level 99) .
*/


/*
directive lean
import Soda.se.umu.cs.soda.prototype.example.market.core.MyList
*/

type Money = Int

trait Item
{

  def   owner : Nat
  def   price : Money

}

case class Item_ (owner : Nat, price : Money) extends Item

object Item {
  def mk (owner : Nat) (price : Money) : Item =
    Item_ (owner, price)
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



  private lazy val _mm : MyList = MyList .mk

/*
  directive lean
  notation "_mm.get" => MyList.get
  notation "_mm.set" => MyList.set
  notation "_mm.foldl" => MyList.foldl
  notation "_mm.append" => MyList.append
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

  private def _deposit_into_known_account (accounts : List [Money] ) (user_id : Nat) (amount : Money)
      : List [Money] =
    (_mm .get [Money] (accounts) (user_id) ) match  {
      case Some (previous_balance) =>
        _mm .set [Money] (accounts) (user_id) (previous_balance + amount)
      case None => accounts
    }

  private def _deposit_into_accounts (accounts : List [Money] ) (user_id : Nat) (amount : Money)
      : List [Money] =
    if ( user_id == accounts .length
    ) _mm .append [Money] (accounts) (amount)
    else _deposit_into_known_account (accounts) (user_id) (amount)

  def deposit (m : Market) (user_id : Nat) (amount : Money) : Market =
    if ( amount >= 0
    ) Market .mk (_deposit_into_accounts (m .accounts) (user_id) (amount) ) (m .items)
    else m

  private def _reassign_item (items : List [Item] ) (item_id : Nat) (user_id: Nat)
      : List [Item] =
    (_mm .get [Item] (items) (item_id) ) match  {
      case Some (item) =>
        _mm .set [Item] (items) (item_id) (Item .mk (user_id) (item .price) )
      case None => items
    }

  private def _assign_to_user (items : List [Item] ) (item_id : Nat) (user_id: Nat)
      : List [Item] =
    if ( item_id == items .length
    ) _mm .append [Item] (items) (Item .mk (user_id) (0) )
    else _reassign_item (items) (item_id) (user_id)

  def assign (m : Market) (item_id : Nat) (user_id : Nat) : Market =
    Market .mk (m .accounts) (_assign_to_user (m .items) (item_id) (user_id) )

  private def _price_item (items : List [Item] ) (item_id : Nat) (p : Money)
      : List [Item] =
    (_mm .get [Item] (items) (item_id) ) match  {
      case Some (item) =>
        _mm .set [Item] (items) (item_id) (Item .mk (item .owner) (p) )
      case None => items
    }

  def price_item (m : Market) (item_id : Nat) (p : Money) : Market =
    Market .mk (m .accounts) (_price_item (m .items) (item_id) (p) )

  def is_advertised (market : Market) (item_id : Nat) : Boolean =
    (_mm .get [Item] (market .items) (item_id) ) match  {
      case Some (item) =>
        item .price > 0
      case None => false
    }

  private def _remove_ad (items : List [Item] ) (item_id : Nat) : List [Item] =
    (_mm .get [Item] (items) (item_id) ) match  {
      case Some (item) =>
        _mm .set [Item] (items) (item_id) (Item .mk (item .owner) (0) )
      case None => items
    }

  def remove_ad (market : Market) (item_id : Nat) : Market =
    Market .mk (market .accounts) (_remove_ad (market .items) (item_id) )

  private def _transfer_with_balances (accounts : List [Money] ) (origin : Nat) (target : Nat)
      (amount : Money) (origin_balance : Money) (target_balance : Money) : List [Money] =
    _mm .set [Money] (_mm .set [Money] (accounts)
      (origin) (origin_balance - amount) ) (target) (target_balance + amount)

  private def _transfer_if_balance_is_sufficient (accounts : List [Money] ) (origin : Nat) (target : Nat)
      (amount : Money) (origin_balance : Money) (target_balance : Money) : List [Money] =
      if ( origin_balance >= amount
      ) _mm .set [Money] (_mm .set [Money] (accounts)
        (origin) (origin_balance - amount) ) (target) (target_balance + amount)
      else accounts

  private def _transfer_with (accounts : List [Money] ) (origin : Nat) (target : Nat) (amount : Money)
      (origin_balance : Money) : List [Money] =
    (_mm .get [Money] (accounts) (target) ) match  {
      case Some (target_balance) =>
        _transfer_if_balance_is_sufficient (accounts) (origin) (target)
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

  private def _sell_if_for_sale (market : Market) (item_id : Nat) (buyer : Nat) (item : Item) : Market =
    if ( item .price > 0
    )
      Market .mk (
        _transfer (market .accounts) (buyer) (item .owner) (item .price) ) (
        _mm .set [Item] (market .items) (item_id) (Item .mk (buyer) (0) )
      )
    else market

  def sell (market : Market) (item_id : Nat) (buyer : Nat) : Market =
    (_mm .get [Item] (market .items) (item_id) ) match  {
      case Some (item) =>
        _sell_if_for_sale (market) (item_id) (buyer) (item)
      case None => market
    }

  private def _sum_pair (a : Money) (b : Money) : Money =
    a + b

  def assets (market : Market) : Money =
    _mm .foldl [Money, Money] (market .accounts) (0) (_sum_pair)

}

case class MarketMod_ () extends MarketMod

object MarketMod {
  def mk : MarketMod =
    MarketMod_ ()
}


trait OperationProcessor
{



  private lazy val _mm : MyList = MyList .mk

  def compute_next (maybe_market : Option [Market] ) (op : Operation) : Option [Market] =
    op .process (maybe_market)

  def process (maybe_market : Option [Market] ) (operations : List [Operation] ) : Option [Market] =
    _mm .foldl [Operation, Option [Market] ] (operations) (maybe_market) (compute_next)

}

case class OperationProcessor_ () extends OperationProcessor

object OperationProcessor {
  def mk : OperationProcessor =
    OperationProcessor_ ()
}

trait MarketBuilder
{



  lazy val operation_processor = OperationProcessor .mk

  lazy val empty_market = Market .mk (List [Money] () ) (List [Item] () )

  def build (operations : List [Operation] ) : Option [Market] =
    operation_processor
      .process (Some (empty_market) ) (operations)

}

case class MarketBuilder_ () extends MarketBuilder

object MarketBuilder {
  def mk : MarketBuilder =
    MarketBuilder_ ()
}


/*
directive lean
import Soda.se.umu.cs.soda.prototype.example.market.core.Basic
*/

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

  private def _tailrec_range (n : Nat) (list : List [Nat] ) : List [Nat] =
    n match  {
      case Succ_ (k) => _tailrec_range (k) ( (k) :: (list) )
      case _otherwise => list
    }

  def range (length : Nat) : List [Nat] =
    if ( (0 <= length)
    ) _tailrec_range (length) (Nil)
    else Nil

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

  def append [A ] (first : List [A] ) (element : A) : List [A] =
    reverse_fl [A]  (element :: reverse_fl [A] (first) )

  def monus1 (index : Nat) : Nat =
    index match  {
      case Succ_ (k) => k
      case _otherwise => 0
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

  def get_fl [A ] (list : List [A] ) (index : Nat) : Option [A] =
    (foldl [A, IndexOption [A] ] (list) (
      IndexOption .mk (0) (None) ) (
         (accum : IndexOption [A] ) =>
           (elem : A) =>
            if ( (accum .current_index == index)
            ) IndexOption .mk (accum .current_index + 1) (Some (elem) )
            else IndexOption .mk (accum .current_index + 1) (accum .maybe_elem)
      )
    ) .maybe_elem

  private def _tailrec_get_def [A ] (list : List [A] ) (index : Nat) (current : Nat) : Option [A] =
    list match  {
      case Nil => None
      case (head) :: (tail) =>
        if ( current == index
        ) Some (head)
        else _tailrec_get_def [A] (tail) (index) (Succ_ (current) )
    }

  def get_def [A ] (list : List [A] ) (index : Nat) : Option [A] =
    _tailrec_get_def [A] (list) (index) (0)

  def get [A ] (list : List [A] ) (index : Nat) : Option [A] =
    get_def [A] (list) (index)

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

  private def _tailrec_set_def_alt [A ] (list : List [A] ) (index : Nat) (element : A) (current : Nat)
      : List [A] =
    list match  {
      case Nil => Nil
      case (head) :: (tail) =>
        if ( current == index
        ) (element) :: (tail)
        else (head) :: (_tailrec_set_def_alt [A] (tail) (index) (element) (Succ_ (current) ) )
    }

  def set_def [A ] (list : List [A] ) (index : Nat) (element : A) : List [A] =
    list match  {
      case Nil => Nil
      case (head) :: (tail) =>
        if ( index == 0
        ) (element) :: (tail)
        else (head) :: (set_def [A] (tail) (monus1 (index) ) (element) )
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
         simp
         rewrite [length_def]
         rfl
       | succ k =>
         rewrite [monus1]
         simp
         rewrite [length_def]
         rewrite [ih]
         rfl
*/

  def set [A ] (list : List [A] ) (index : Nat) (element : A) : List [A] =
    if ( (0 <= index)
    ) set_def [A] (list) (index) (element)
    else list

}

case class MyList_ () extends MyList

object MyList {
  def mk : MyList =
    MyList_ ()
}


trait OperationType
{

  def   ordinal : Int
  def   name : String

}

case class OperationType_ (ordinal : Int, name : String) extends OperationType

object OperationType {
  def mk (ordinal : Int) (name : String) : OperationType =
    OperationType_ (ordinal, name)
}

trait OperationEnum
{



  lazy val undefined = OperationType .mk (0) ("undefined")

  lazy val deposit = OperationType .mk (1) ("deposit")

  lazy val assign = OperationType .mk (2) ("assign")

  lazy val price = OperationType .mk (3) ("price")

  lazy val sell = OperationType .mk (4) ("sell")

  lazy val values = List (undefined , deposit , assign , price , sell)

}

case class OperationEnum_ () extends OperationEnum

object OperationEnum {
  def mk : OperationEnum =
    OperationEnum_ ()
}

trait Operation
{

  def   op_type : OperationType
  def   process : Option [Market] => Option [Market]

}

case class Operation_ (op_type : OperationType, process : Option [Market] => Option [Market]) extends Operation

object Operation {
  def mk (op_type : OperationType) (process : Option [Market] => Option [Market]) : Operation =
    Operation_ (op_type, process)
}

trait OpUndefined
  extends
    Operation
{



  lazy val op_type = OperationEnum .mk .undefined

  lazy val process : Option [Market] => Option [Market] =
     maybe_market => None

}

case class OpUndefined_ () extends OpUndefined

object OpUndefined {
  def mk : OpUndefined =
    OpUndefined_ ()
}

trait OpDeposit
  extends
    Operation
{

  def   user_id : Nat
  def   amount : Money

  lazy val op_type = OperationEnum .mk .deposit

  def process_market (m : Market) : Option [Market] =
    if ( (user_id <= m .accounts .length) && (amount >= 0)
    ) Some (MarketMod .mk .deposit (m) (user_id) (amount) )
    else None

  def process_maybe_market (maybe_market : Option [Market] ) : Option [Market] =
    maybe_market match  {
      case Some (market) => process_market (market)
      case None => None
    }

  lazy val process : Option [Market] => Option [Market] =
     maybe_market =>
      process_maybe_market (maybe_market)

}

case class OpDeposit_ (user_id : Nat, amount : Money) extends OpDeposit

object OpDeposit {
  def mk (user_id : Nat) (amount : Money) : OpDeposit =
    OpDeposit_ (user_id, amount)
}

trait OpAssign
  extends
    Operation
{

  def   item_id : Nat
  def   user_id : Nat

  lazy val op_type = OperationEnum .mk .assign

  def process_market (m : Market) : Option [Market] =
    if ( (item_id <= m .items. length) && (user_id < m .accounts .length)
    ) Some (MarketMod .mk .assign (m) (item_id) (user_id) )
    else None

  def process_maybe_market (maybe_market : Option [Market] ) : Option [Market] =
    maybe_market match  {
      case Some (market) => process_market (market)
      case None => None
    }

  lazy val process : Option [Market] => Option [Market] =
     maybe_market =>
      process_maybe_market (maybe_market)

}

case class OpAssign_ (item_id : Nat, user_id : Nat) extends OpAssign

object OpAssign {
  def mk (item_id : Nat) (user_id : Nat) : OpAssign =
    OpAssign_ (item_id, user_id)
}

trait OpPrice
  extends
    Operation
{

  def   item_id : Nat
  def   price : Money

  lazy val op_type = OperationEnum .mk .price

  def process_market (m : Market) : Option [Market] =
    if ( (item_id < m .items. length) && (price >= 0)
    ) Some (MarketMod .mk .price_item (m) (item_id) (price) )
    else None

  def process_maybe_market (maybe_market : Option [Market] ) : Option [Market] =
    maybe_market match  {
      case Some (market) => process_market (market)
      case None => None
    }

  lazy val process : Option [Market] => Option [Market] =
     maybe_market =>
      process_maybe_market (maybe_market)

}

case class OpPrice_ (item_id : Nat, price : Money) extends OpPrice

object OpPrice {
  def mk (item_id : Nat) (price : Money) : OpPrice =
    OpPrice_ (item_id, price)
}

trait OpSell
  extends
    Operation
{

  def   item_id : Nat
  def   user_id : Nat

  lazy val op_type = OperationEnum .mk .sell

  def process_market (m : Market) : Option [Market] =
    if ( (item_id < m .items .length) && (user_id < m .accounts .length)
    ) Some (MarketMod .mk .sell (m) (item_id) (user_id) )
    else None

  def process_maybe_market (maybe_market : Option [Market] ) : Option [Market] =
    maybe_market match  {
      case Some (market) => process_market (market)
      case None => None
    }

  lazy val process : Option [Market] => Option [Market] =
     maybe_market =>
      process_maybe_market (maybe_market)

}

case class OpSell_ (item_id : Nat, user_id : Nat) extends OpSell

object OpSell {
  def mk (item_id : Nat) (user_id : Nat) : OpSell =
    OpSell_ (item_id, user_id)
}

