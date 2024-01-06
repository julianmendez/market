package soda.se.umu.cs.soda.prototype.example.market

/*
 * This package contains classes to model a market
 *
 */

trait Package

trait Item
{

  def   owner : Int
  def   price : Int
  def   advertised : Boolean

}

case class Item_ (owner : Int, price : Int, advertised : Boolean) extends Item

trait Market
{

  def   accounts : Seq [Int]
  def   items : Seq [Item]

  def mk_market (new_accounts : Seq [Int] ) (new_items : Seq [Item] ) : Market =
    Market_ (new_accounts, new_items)

  def as_market () : Market =
    mk_market (accounts) (items)

  private def _advertise_item (item : Item) : Item =
    Item_ (item .owner, item .price, true)

  private def _advertise (items : Seq [Item] ) (item_id : Int) : Seq [Item] =
    items .updated (item_id , _advertise_item (items .apply (item_id) ) )

  def advertise (item_id : Int) : Market =
    mk_market (accounts) (_advertise (items) (item_id) )

  private def _hide_item (item : Item) : Item =
    Item_ (item .owner, item .price, false)

  private def _remove_ad (items : Seq [Item] ) (item_id : Int) : Seq [Item] =
    items .updated (item_id , _hide_item (items .apply (item_id) ) )

  def remove_ad (item_id : Int) (price : Int) : Market =
    mk_market (accounts) (_remove_ad (items) (item_id) )

  private def _transfer (accounts0 : Seq [Int] ) (origin : Int) (target : Int) (amount : Int) : Seq [Int] =
    accounts0
      .updated (origin , accounts .apply (origin) - amount)
      .updated (target , accounts .apply (target) + amount)

  private def _give (items0 : Seq [Item] ) (item_id : Int) (buyer : Int) (price : Int)
      : Seq [Item] =
    items0
      .updated (item_id , Item_ (buyer, price, false) )

  private def _sell_item (item : Item) (item_id : Int) (buyer : Int) : Market =
    mk_market (
      _transfer (accounts) (buyer) (item .owner) (item .price) ) (
      _give (items) (item_id) (buyer) (item .price)
    )

  def sell (item_id : Int) (buyer : Int) : Market =
    _sell_item (items .apply (item_id) ) (item_id) (buyer)

}

case class Market_ (accounts : Seq [Int], items : Seq [Item]) extends Market

