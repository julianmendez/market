

notation:max "Boolean" => Bool
notation:max "Some" => some
notation:max "Index" => Nat

/-
directive scala
type Index = Int
-/

namespace Item

class Item where
  Item_ ::
    owner : Index
    price : Int
    advertised : Boolean
  deriving DecidableEq

namespace Item


end Item

open Item

namespace Market

class Market where
  Market_ ::
    accounts : List ( Int )
    items : List ( Item )
  deriving DecidableEq

namespace Market


end Market

open Market

namespace MarketMod

/-
  directive scala
  def get [A] (list : List [A]) (index : Index) : Option [A] =
    list.lift (index)
-/

  def get {A : Type} (list : List (A)) (index : Index) : Option (A) :=
    list.get? (index)

/-
  directive scala
  def set [A] (list : List [A]) (index : Index) (element : A) : List [A] =
    if (index < list.length)
    then list.updated (index , element)
    else list
-/

  def set {A : Type} (list : List (A)) (index : Index) (element : A) : List (A) :=
    if (index < list.length)
    then list.set (index) (element)
    else list

 def   mk_market (new_accounts : List ( Int ) ) (new_items : List ( Item ) ) : Market :=
    Market_ (new_accounts) (new_items)


 def   as_market (market : Market) : Market :=
    mk_market (market.accounts) (market.items)


 def   _advertise_item (item : Item) : Item :=
    Item_ (item.owner) (item.price) (true)


 def   _advertise (items : List ( Item ) ) (item_id : Index) : List ( Item ) :=
    match (get (items) (item_id) ) with
      | Some (item) => set (items) (item_id) (_advertise_item (item) )
      | otherwise => items
    


 def   advertise (market : Market) (item_id : Index) : Market :=
    mk_market (market.accounts) (_advertise (market.items) (item_id) )


 def   _hide_item (item : Item) : Item :=
    Item_ (item.owner) (item.price) (false)


 def   _remove_ad (items : List ( Item ) ) (item_id : Index) : List ( Item ) :=
    match (get (items) (item_id) ) with
      | Some (item) => set (items) (item_id) (_hide_item (item) )
      | otherwise => items
    


 def   remove_ad (market : Market) (item_id : Index) : Market :=
    mk_market (market.accounts) (_remove_ad (market.items) (item_id) )


def   _transfer_with_balances (accounts : List ( Int ) ) (origin : Index) (target : Index)
       (amount : Int) (origin_balance : Int) (target_balance : Int) : List ( Int ) :=
    set (set (accounts) (origin) (origin_balance - amount) ) (target) (target_balance + amount)


def   _transfer_with (accounts : List ( Int ) ) (origin : Index) (target : Index) (amount : Int)
       (origin_balance : Int) : List ( Int ) :=
    match (get (accounts) (target) ) with
      | Some (target_balance) =>
        _transfer_with_balances (accounts) (origin) (target) (amount) (origin_balance) (target_balance)
      | otherwise => accounts
    


def   _transfer (accounts : List ( Int ) ) (origin : Index) (target : Index) (amount : Int)
       : List ( Int ) :=
    match (get (accounts) (origin) ) with
      | Some (origin_balance) =>
        _transfer_with (accounts) (origin) (target) (amount) (origin_balance)
      | None => accounts
    


 def   _give (items : List ( Item ) ) (item_id : Index) (buyer : Index) (price : Int) : List ( Item ) :=
    set (items) (item_id) (Item_ (buyer) (price) (false) )


 def   _sell_item (market : Market) (item : Item) (item_id : Index) (buyer : Index) : Market :=
    mk_market (
      _transfer (market.accounts) (buyer) (item.owner) (item.price) ) (
      _give (market.items) (item_id) (buyer) (item.price)
    )


 def   sell (market : Market) (item_id : Index) (buyer : Index) : Market :=
    match (get (market.items) (item_id) ) with
      | Some (item) =>
        _sell_item (market) (item) (item_id) (buyer)
      | otherwise =>
        market
    


end MarketMod

open MarketMod
