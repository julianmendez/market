package soda.se.umu.cs.soda.prototype.example.market

/*
 * This package contains classes to model a market
 *
 */

trait Package

/*
directive lean
/- Prelude for Soda types. -/
notation:max "Boolean" => Bool
notation:max "None" => none
notation:max "Some" => some
notation:max "Nil" => []
*/

type Nat = Int

type Index = Nat

type Money = Int

trait Item
{

  def   owner : Index
  def   price : Money
  def   advertised : Boolean

}

case class Item_ (owner : Index, price : Money, advertised : Boolean) extends Item

trait Market
{

  def   accounts : List [Money]
  def   items : List [Item]

}

case class Market_ (accounts : List [Money], items : List [Item]) extends Market

trait MarketMod
{

  private def _tailrec_fold [A , B ] (sequence : List [A] ) (current : B)
      (next_value : B => A => B) : B =
    sequence match  {
      case Nil => current
      case (head) :: (tail) =>
        _tailrec_fold [A, B] (tail) (next_value (current) (head) ) (next_value)
    }

  def fold [A , B ] (sequence : List [A] ) (initial_value : B)
      (next_value : B => A => B) : B =
    _tailrec_fold [A, B] (sequence) (initial_value) (next_value)

  private def _tailrec_length [A ] (list : List [A] ) (accum : Index) : Index =
    list match  {
      case Nil => accum
      case (head) :: (tail) =>
        _tailrec_length [A] (tail) (accum + 1)
    }

  def length [A ] (list : List [A] ) : Index =
    _tailrec_length [A] (list) (0)

  private def _tailrec_reverse [A ] (list : List [A] ) (accum : List [A] ) : List [A] =
    list match  {
      case Nil => accum
      case (head) :: (tail) => _tailrec_reverse [A] (tail) ( (head) :: (accum) )
    }

  def reverse [A ] (list : List [A] ) : List [A] =
    _tailrec_reverse [A] (list) (Nil)

/*
  directive lean
  theorem
    len_rev (A : Type) (list : List (A) )
      : length (A) (reverse (A) (list)) = length (A) (list) := by
    induction list with
    | nil =>
        constructor
    | cons head tail h1 =>
        rewrite [length, length, reverse] at h1
        rewrite [length, length, reverse]
        rewrite [_tailrec_reverse]
        sorry
*/

  private def _tailrec_concat [A ] (rev_first : List [A] ) (second : List [A] ) : List [A] =
    rev_first match  {
      case Nil => second
      case (head) :: (tail) => _tailrec_concat [A] (tail) ( (head) :: (second) )
    }

  def concat [A ] (first : List [A] ) (second : List [A] ) : List [A] =
    _tailrec_concat [A] (reverse [A] (first) ) (second)

  def monus1 (index : Index) : Index =
    if ( index <= 0
    ) 0
    else index - 1

  private def _tailrec_get [A ] (list : List [A] ) (index : Index) : Option [A] =
    list match  {
      case Nil => None
      case (head) :: (tail) =>
        if ( index == 0
        ) Some (head)
        else _tailrec_get [A] (tail) (monus1 (index) )
    }

  def get [A ] (list : List [A] ) (index : Index) : Option [A] =
    _tailrec_get [A] (list) (index)

  private def _tailrec_set [A ] (list : List [A] ) (accum : List [A] ) (index : Index)
      (element : A) : List [A] =
    list match  {
      case Nil => reverse [A] (accum)
      case (head) :: (tail) =>
        if ( index == 0
        ) concat [A] (reverse [A] (accum) ) ( (element) :: (tail) )
        else _tailrec_set [A] (tail) ( (head) :: (accum) ) (monus1 (index) ) (element)
    }

  def set [A ] (list : List [A] ) (index : Index) (element : A) : List [A] =
    _tailrec_set [A] (list) (Nil) (index) (element)

  def mk_Market (new_accounts : List [Money] ) (new_items : List [Item] ) : Market =
    Market_ (new_accounts, new_items)

  def as_market (market : Market) : Market =
    mk_Market (market .accounts) (market .items)

  private def _advertise (items : List [Item] ) (item_id : Index) : List [Item] =
    (get [Item] (items) (item_id) ) match  {
      case Some (item) =>
        set [Item] (items) (item_id) (Item_ (item .owner, item .price, true) )
      case otherwise => items
    }

  def advertise (market : Market) (item_id : Index) : Market =
    mk_Market (market .accounts) (_advertise (market .items) (item_id) )

  private def _remove_ad (items : List [Item] ) (item_id : Index) : List [Item] =
    (get [Item] (items) (item_id) ) match  {
      case Some (item) =>
        set [Item] (items) (item_id) (Item_ (item .owner, item .price, false) )
      case otherwise => items
    }

  def remove_ad (market : Market) (item_id : Index) : Market =
    mk_Market (market .accounts) (_remove_ad (market .items) (item_id) )

  private def _transfer_with_balances (accounts : List [Money] ) (origin : Index) (target : Index)
      (amount : Money) (origin_balance : Money) (target_balance : Money) : List [Money] =
    set [Money] (set [Money] (accounts)
      (origin) (origin_balance - amount) ) (target) (target_balance + amount)

  private def _transfer_with (accounts : List [Money] ) (origin : Index) (target : Index) (amount : Money)
      (origin_balance : Money) : List [Money] =
    (get [Money] (accounts) (target) ) match  {
      case Some (target_balance) =>
        _transfer_with_balances (accounts) (origin) (target)
          (amount) (origin_balance) (target_balance)
      case otherwise => accounts
    }

  private def _transfer (accounts : List [Money] ) (origin : Index) (target : Index) (amount : Money)
      : List [Money] =
    (get [Money] (accounts) (origin) ) match  {
      case Some (origin_balance) =>
        _transfer_with (accounts) (origin) (target) (amount) (origin_balance)
      case None => accounts
    }

  def sell (market : Market) (item_id : Index) (buyer : Index) : Market =
    (get [Item] (market .items) (item_id) ) match  {
      case Some (item) =>
        mk_Market (
          _transfer (market .accounts) (buyer) (item .owner) (item .price) ) (
          set [Item] (market .items) (item_id) (Item_ (buyer, item .price, false) )
        )
      case otherwise =>
        market
    }

  private def _sum_pair (a : Money) (b : Money) : Money =
    a + b

  def assets (market : Market) : Money =
    fold [Money, Money] (market .accounts) (0) (_sum_pair)

/*
  directive lean
  theorem
    lemma_set_keeps_length_1 (A : Type) (index : Index) (element : A) :
      ((Nil).set (index) (element) ).length = 0 :=
      by constructor
*/

/*
  directive lean
  theorem
    lemma_set_keeps_length_2 (A : Type) (head : A) (tail : List (A) ) (element : A) :
      ((head :: tail).set (0) (head)).length = ((element :: tail).set (0) (element)).length :=
      by constructor
*/

/*
  directive lean
  theorem
    set_keeps_length (A : Type) (list : List (A)) (index : Index) (element : A) :
      (list.set (index) (element) ).length = list.length :=
    match list with
      | Nil => lemma_set_keeps_length_1 (A) (index) (element)
      | (head) :: (tail) =>
        match index with
          | 0 => lemma_set_keeps_length_2 (A) (head) (tail) (element)
          | k + 1 => sorry
*/

/*
  directive lean
  theorem
    conservation_of_items_after_sell_operation (market : Market) (item_id : Index) (buyer : Index) :
       (sell (market) (item_id) (buyer) ).items.length = market.items.length :=
    sorry
*/

/*
  directive lean
  theorem
    lemma_fold (accounts : List (Money) ) (items : List (Item) ) (item_id : Index) (buyer : Index) :
     fold (Money) (Money) ( (sell (Market_ (accounts, items)) (item_id) (buyer) ).accounts) (0) (_sum_pair) =
       fold (Money) (Money) (accounts) (0) (_sum_pair) :=
         sorry
*/

/*
  directive lean
  theorem
    conservation_of_money_after_sell_operation (market : Market) (item_id : Index) (buyer : Index) :
      assets (sell (market) (item_id) (buyer) ) = assets (market) :=
    lemma_fold (market.accounts) (market.items) (item_id) (buyer)
*/

}

case class MarketMod_ () extends MarketMod

