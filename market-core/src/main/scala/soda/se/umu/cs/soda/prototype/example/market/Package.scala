package soda.se.umu.cs.soda.prototype.example.market

/*
 * This package contains classes to model a market
 *
 */

trait Package



/*
directive lean
notation:max "Boolean" => Bool
*/

type Index = Int

/*
directive lean
def Index : Type := Nat deriving DecidableEq
*/

trait Item
{

  def   owner : Index
  def   price : Int
  def   advertised : Boolean

}

case class Item_ (owner : Index, price : Int, advertised : Boolean) extends Item

trait Market
{

  def   accounts : List [Int]
  def   items : List [Item]

}

case class Market_ (accounts : List [Int], items : List [Item]) extends Market

trait MarketMod
{

  def get [A] (list : List [A]) (index : Index) : A =
    list.apply (index)

/*
  directive lean
  def get {A : Type} (list : List (A)) (index : Index) : A :=
    list.get! (index)
*/

  def set [A] (list : List [A]) (index : Index) (element : A) : List [A] =
    list.updated (index , element)

/*
  directive lean
  def set {A : Type} (list : List (A)) (index : Index) (element : A) : List (A) :=
    list.set (index) (element)
*/

  def mk_market (new_accounts : List [Int] ) (new_items : List [Item] ) : Market =
    Market_ (new_accounts, new_items)

  def as_market (market : Market) : Market =
    mk_market (market .accounts) (market .items)

  private def _advertise_item (item : Item) : Item =
    Item_ (item .owner, item .price, true)

  private def _advertise (items : List [Item] ) (item_id : Index) : List [Item] =
    set (items) (item_id) (_advertise_item (get (items) (item_id) ) )

  def advertise (market : Market) (item_id : Index) : Market =
    mk_market (market .accounts) (_advertise (market .items) (item_id) )

  private def _hide_item (item : Item) : Item =
    Item_ (item .owner, item .price, false)

  private def _remove_ad (items : List [Item] ) (item_id : Index) : List [Item] =
    set (items) (item_id) (_hide_item (get (items) (item_id) ) )

  def remove_ad (market : Market) (item_id : Index) : Market =
    mk_market (market .accounts) (_remove_ad (market .items) (item_id) )

  private def _transfer (accounts : List [Int] ) (origin : Index) (target : Index) (amount : Int)
      : List [Int] =
    set (
      set (accounts) (origin) ( (get (accounts) (origin) ) - amount)
    ) (target) ( (get (accounts) (target) ) + amount)

  private def _give (items : List [Item] ) (item_id : Index) (buyer : Index) (price : Int) : List [Item] =
    set (items) (item_id) (Item_ (buyer, price, false) )

  private def _sell_item (market : Market) (item : Item) (item_id : Index) (buyer : Index) : Market =
    mk_market (
      _transfer (market .accounts) (buyer) (item .owner) (item .price) ) (
      _give (market .items) (item_id) (buyer) (item .price)
    )

  def sell (market : Market) (item_id : Index) (buyer : Index) : Market =
    _sell_item (market) (get (market .items) (item_id) ) (item_id) (buyer)

}

case class MarketMod_ () extends MarketMod

