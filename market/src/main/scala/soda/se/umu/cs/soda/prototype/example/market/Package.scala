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

trait Item
{

  def   owner : Int
  def   price : Int
  def   advertised : Boolean

}

case class Item_ (owner : Int, price : Int, advertised : Boolean) extends Item

trait Market
{

  def   accounts : List [Int]
  def   items : List [Item]

}

case class Market_ (accounts : List [Int], items : List [Item]) extends Market

trait MarketMod
{

  def mk_market (new_accounts : List [Int] ) (new_items : List [Item] ) : Market =
    Market_ (new_accounts, new_items)

  def as_market (market : Market) : Market =
    mk_market (market .accounts) (market .items)

  private def _advertise_item (item : Item) : Item =
    Item_ (item .owner, item .price, true)

  private def _advertise (items : List [Item] ) (item_id : Int) : List [Item] =
    items .updated (item_id , _advertise_item (items .apply (item_id) ) )

  def advertise (market : Market) (item_id : Int) : Market =
    mk_market (market .accounts) (_advertise (market .items) (item_id) )

  private def _hide_item (item : Item) : Item =
    Item_ (item .owner, item .price, false)

  private def _remove_ad (items : List [Item] ) (item_id : Int) : List [Item] =
    items .updated (item_id , _hide_item (items .apply (item_id) ) )

  def remove_ad (market : Market) (item_id : Int) : Market =
    mk_market (market .accounts) (_remove_ad (market .items) (item_id) )

  private def _transfer (accounts : List [Int] ) (origin : Int) (target : Int) (amount : Int)
      : List [Int] =
    accounts
      .updated (origin , accounts .apply (origin) - amount)
      .updated (target , accounts .apply (target) + amount)

  private def _give (items : List [Item] ) (item_id : Int) (buyer : Int) (price : Int) : List [Item] =
    items
      .updated (item_id , Item_ (buyer, price, false) )

  private def _sell_item (market : Market) (item : Item) (item_id : Int) (buyer : Int) : Market =
    mk_market (
      _transfer (market .accounts) (buyer) (item .owner) (item .price) ) (
      _give (market .items) (item_id) (buyer) (item .price)
    )

  def sell (market : Market) (item_id : Int) (buyer : Int) : Market =
    _sell_item (market) (market .items .apply (item_id) ) (item_id) (buyer)

}

case class MarketMod_ () extends MarketMod

