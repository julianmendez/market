/- Prelude for Soda types. -/
notation:max "Boolean" => Bool
notation:max "None" => none
notation:max "Some" => some
notation:max "Nil" => []

/-
directive scala
type Nat = Int
-/

notation:max "Index" => Nat

notation:max "Money" => Int

namespace Item

class Item where
  Item_ ::
    owner : Index
    price : Money
    advertised : Boolean
  deriving DecidableEq

namespace Item


end Item

open Item

namespace Market

class Market where
  Market_ ::
    accounts : List ( Money )
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

def   _tailrec_fold ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
       (next_value : B -> A -> B) : B :=
    match sequence with
      | Nil => current
      | (head) :: (tail) =>
        _tailrec_fold ( A ) ( B ) (tail) (next_value (current) (head) ) (next_value)
    


def   fold ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (initial_value : B)
       (next_value : B -> A -> B) : B :=
    _tailrec_fold ( A ) ( B ) (sequence) (initial_value) (next_value)


 def   mk_market (new_accounts : List ( Money ) ) (new_items : List ( Item ) ) : Market :=
    Market_ (new_accounts) (new_items)


 def   as_market (market : Market) : Market :=
    mk_market (market.accounts) (market.items)


 def   _advertise (items : List ( Item ) ) (item_id : Index) : List ( Item ) :=
    match (get (items) (item_id) ) with
      | Some (item) =>
        set (items) (item_id) (Item_ (item.owner) (item.price) (true) )
      | otherwise => items
    


 def   advertise (market : Market) (item_id : Index) : Market :=
    mk_market (market.accounts) (_advertise (market.items) (item_id) )


 def   _remove_ad (items : List ( Item ) ) (item_id : Index) : List ( Item ) :=
    match (get (items) (item_id) ) with
      | Some (item) =>
        set (items) (item_id) (Item_ (item.owner) (item.price) (false) )
      | otherwise => items
    


 def   remove_ad (market : Market) (item_id : Index) : Market :=
    mk_market (market.accounts) (_remove_ad (market.items) (item_id) )


def   _transfer_with_balances (accounts : List ( Money ) ) (origin : Index) (target : Index)
       (amount : Money) (origin_balance : Money) (target_balance : Money) : List ( Money ) :=
    set (set (accounts) (origin) (origin_balance - amount) ) (target) (target_balance + amount)


def   _transfer_with (accounts : List ( Money ) ) (origin : Index) (target : Index) (amount : Money)
       (origin_balance : Money) : List ( Money ) :=
    match (get (accounts) (target) ) with
      | Some (target_balance) =>
        _transfer_with_balances (accounts) (origin) (target) (amount) (origin_balance) (target_balance)
      | otherwise => accounts
    


def   _transfer (accounts : List ( Money ) ) (origin : Index) (target : Index) (amount : Money)
       : List ( Money ) :=
    match (get (accounts) (origin) ) with
      | Some (origin_balance) =>
        _transfer_with (accounts) (origin) (target) (amount) (origin_balance)
      | None => accounts
    


 def   sell (market : Market) (item_id : Index) (buyer : Index) : Market :=
    match (get (market.items) (item_id) ) with
      | Some (item) =>
        mk_market (
          _transfer (market.accounts) (buyer) (item.owner) (item.price) ) (
          set (market.items) (item_id) (Item_ (buyer) (item.price) (false) )
        )
      | otherwise =>
        market
    


 def   _sum_pair (a : Money) (b : Money) : Money :=
    a + b


 def   assets (market : Market) : Money :=
    fold ( Money ) ( Money ) (market.accounts) (0) (_sum_pair)


  theorem
    money_conservation_after_sell (market : Market) (item_id : Index) (buyer : Index) :
      assets (sell (market) (item_id) (buyer) ) = assets (market) :=
    by sorry

end MarketMod

open MarketMod
