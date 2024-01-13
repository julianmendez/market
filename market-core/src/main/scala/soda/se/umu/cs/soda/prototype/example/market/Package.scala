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

  def get [A] (list : List [A]) (index : Index) : Option [A] =
    list.lift (index)

/*
  directive lean
  def get {A : Type} (list : List (A)) (index : Index) : Option (A) :=
    list.get? (index)
*/

  def set [A] (list : List [A]) (index : Index) (element : A) : List [A] =
    if (index < list.length)
    then list.updated (index , element)
    else list

/*
  directive lean
  def set {A : Type} (list : List (A)) (index : Index) (element : A) : List (A) :=
    if (index < list.length)
    then list.set (index) (element)
    else list
*/

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

  def mk_market (new_accounts : List [Money] ) (new_items : List [Item] ) : Market =
    Market_ (new_accounts, new_items)

  def as_market (market : Market) : Market =
    mk_market (market .accounts) (market .items)

  private def _advertise (items : List [Item] ) (item_id : Index) : List [Item] =
    (get (items) (item_id) ) match  {
      case Some (item) =>
        set (items) (item_id) (Item_ (item .owner, item .price, true) )
      case otherwise => items
    }

  def advertise (market : Market) (item_id : Index) : Market =
    mk_market (market .accounts) (_advertise (market .items) (item_id) )

  private def _remove_ad (items : List [Item] ) (item_id : Index) : List [Item] =
    (get (items) (item_id) ) match  {
      case Some (item) =>
        set (items) (item_id) (Item_ (item .owner, item .price, false) )
      case otherwise => items
    }

  def remove_ad (market : Market) (item_id : Index) : Market =
    mk_market (market .accounts) (_remove_ad (market .items) (item_id) )

  private def _transfer_with_balances (accounts : List [Money] ) (origin : Index) (target : Index)
      (amount : Money) (origin_balance : Money) (target_balance : Money) : List [Money] =
    set (set (accounts) (origin) (origin_balance - amount) ) (target) (target_balance + amount)

  private def _transfer_with (accounts : List [Money] ) (origin : Index) (target : Index) (amount : Money)
      (origin_balance : Money) : List [Money] =
    (get (accounts) (target) ) match  {
      case Some (target_balance) =>
        _transfer_with_balances (accounts) (origin) (target) (amount) (origin_balance) (target_balance)
      case otherwise => accounts
    }

  private def _transfer (accounts : List [Money] ) (origin : Index) (target : Index) (amount : Money)
      : List [Money] =
    (get (accounts) (origin) ) match  {
      case Some (origin_balance) =>
        _transfer_with (accounts) (origin) (target) (amount) (origin_balance)
      case None => accounts
    }

  def sell (market : Market) (item_id : Index) (buyer : Index) : Market =
    (get (market .items) (item_id) ) match  {
      case Some (item) =>
        mk_market (
          _transfer (market .accounts) (buyer) (item .owner) (item .price) ) (
          set (market .items) (item_id) (Item_ (buyer, item .price, false) )
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
    money_conservation_after_sell (market : Market) (item_id : Index) (buyer : Index) :
      assets (sell (market) (item_id) (buyer) ) = assets (market) :=
    by sorry
*/

}

case class MarketMod_ () extends MarketMod

