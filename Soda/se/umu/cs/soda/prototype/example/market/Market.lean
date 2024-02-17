

import Soda.se.umu.cs.soda.prototype.example.market.MyList

notation "Money" => Int

class Item

where
  mk ::
    owner : Nat
    price : Money
    advertised : Bool
  deriving DecidableEq

namespace Item


end Item

notation "Item_" => Item.mk

class Market

where
  mk ::
    accounts : List ( Money )
    items : List ( Item )
  deriving DecidableEq

namespace Market


end Market

notation "Market_" => Market.mk

class MarketMod

where
  mk ::
    bit : Bool
  deriving DecidableEq

namespace MarketMod


 private def   _mm : MyList := MyList_ (true)


  notation "_mm.get" => MyList.get
  notation "_mm.set" => MyList.set
  notation "_mm.foldl" => MyList.foldl

 def   as_market (market : Market) : Market :=
    Market.mk (market.accounts) (market.items)


 def   get_items (market : Market) : List ( Item ) :=
    match market with
      | Market_ (_accounts) (items) => items
    


  theorem
    proj_items (market : Market) (accounts : List (Money)) (items : List (Item))
      : (market = Market_ (accounts) (items) ) -> get_items (market) = items := by
    rewrite [get_items]
    cases market
    intro h1
    rewrite [h1]
    simp

 private def   _advertise (items : List ( Item ) ) (item_id : Nat) : List ( Item ) :=
    match (_mm.get ( Item ) (items) (item_id) ) with
      | Option.some (item) =>
        _mm.set ( Item ) (items) (item_id) (Item_ (item.owner) (item.price) (true) )
      | Option.none => items
    


 def   advertise (market : Market) (item_id : Nat) : Market :=
    Market.mk (market.accounts) (_advertise (market.items) (item_id) )


 private def   _remove_ad (items : List ( Item ) ) (item_id : Nat) : List ( Item ) :=
    match (_mm.get ( Item ) (items) (item_id) ) with
      | Option.some (item) =>
        _mm.set ( Item ) (items) (item_id) (Item_ (item.owner) (item.price) (false) )
      | Option.none => items
    


 def   remove_ad (market : Market) (item_id : Nat) : Market :=
    Market.mk (market.accounts) (_remove_ad (market.items) (item_id) )


private def   _transfer_with_balances (accounts : List ( Money ) ) (origin : Nat) (target : Nat)
       (amount : Money) (origin_balance : Money) (target_balance : Money) : List ( Money ) :=
    _mm.set ( Money ) (_mm.set ( Money ) (accounts)
      (origin) (origin_balance - amount) ) (target) (target_balance + amount)


private def   _transfer_with (accounts : List ( Money ) ) (origin : Nat) (target : Nat) (amount : Money)
       (origin_balance : Money) : List ( Money ) :=
    match (_mm.get ( Money ) (accounts) (target) ) with
      | Option.some (target_balance) =>
        _transfer_with_balances (accounts) (origin) (target)
          (amount) (origin_balance) (target_balance)
      | Option.none => accounts
    


private def   _transfer (accounts : List ( Money ) ) (origin : Nat) (target : Nat) (amount : Money)
       : List ( Money ) :=
    match (_mm.get ( Money ) (accounts) (origin) ) with
      | Option.some (origin_balance) =>
        _transfer_with (accounts) (origin) (target) (amount) (origin_balance)
      | Option.none => accounts
    


 def   sell (market : Market) (item_id : Nat) (buyer : Nat) : Market :=
    match (_mm.get ( Item ) (market.items) (item_id) ) with
      | Option.some (item) =>
        Market.mk (
          _transfer (market.accounts) (buyer) (item.owner) (item.price) ) (
          _mm.set ( Item ) (market.items) (item_id) (Item_ (buyer) (item.price) (false) )
        )
      | Option.none => market
    


 private def   _sum_pair (a : Money) (b : Money) : Money :=
    a + b


 def   assets (market : Market) : Money :=
    _mm.foldl ( Money ) ( Money ) (market.accounts) (0) (_sum_pair)


end MarketMod

notation "MarketMod_" => MarketMod.mk
