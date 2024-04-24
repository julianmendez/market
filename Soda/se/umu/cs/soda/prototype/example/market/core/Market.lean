

import Soda.se.umu.cs.soda.prototype.example.market.core.MyList

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

private def   _deposit_into_known_account (accounts : List ( Money ) ) (user_id : Nat) (amount : Money)
       : List ( Money ) :=
    match (_mm.get ( Money ) (accounts) (user_id) ) with
      | Option.some (previous_balance) =>
        _mm.set ( Money ) (accounts) (user_id) (previous_balance + amount)
      | Option.none => accounts
    


private def   _deposit_into_accounts (accounts : List ( Money ) ) (user_id : Nat) (amount : Money)
       : List ( Money ) :=
    if user_id == accounts.length
    then amount :: accounts
    else _deposit_into_known_account (accounts) (user_id) (amount)


 def   deposit (m : Market) (user_id : Nat) (amount : Money) : Market :=
    if amount >= 0
    then Market.mk (_deposit_into_accounts (m.accounts) (user_id) (amount) ) (m.items)
    else m


private def   _reassign_item (items : List ( Item ) ) (item_id : Nat) (user_id: Nat)
       : List ( Item ) :=
    match (_mm.get ( Item ) (items) (item_id) ) with
      | Option.some (item) =>
        _mm.set ( Item ) (items) (item_id) (Item.mk (user_id) (item.price) (item.advertised) )
      | Option.none => items
    


private def   _assign_to_user (items : List ( Item ) ) (item_id : Nat) (user_id: Nat)
       : List ( Item ) :=
    if item_id == items.length
    then Item.mk (user_id) (0) (false) :: items
    else _reassign_item (items) (item_id) (user_id)


 def   assign (m : Market) (item_id : Nat) (user_id : Nat) : Market :=
    Market.mk (m.accounts) (_assign_to_user (m.items) (item_id) (user_id) )


 def   auto_advertise (p : Money) : Bool :=
    p > 0


private def   _price_item (items : List ( Item ) ) (item_id : Nat) (p : Money)
       : List ( Item ) :=
    match (_mm.get ( Item ) (items) (item_id) ) with
      | Option.some (item) =>
        _mm.set ( Item ) (items) (item_id) (Item.mk (item.owner) (p) (auto_advertise (p) ) )
      | Option.none => items
    


 def   price_item (m : Market) (item_id : Nat) (p : Money) : Market :=
    Market.mk (m.accounts) (_price_item (m.items) (item_id) (p) )


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
