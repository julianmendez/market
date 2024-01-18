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

private def   _tailrec_fold ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
       (next_value : B -> A -> B) : B :=
    match sequence with
      | [] => current
      | (head) :: (tail) =>
        _tailrec_fold ( A ) ( B ) (tail) (next_value (current) (head) ) (next_value)
    


def   fold ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (initial_value : B)
       (next_value : B -> A -> B) : B :=
    _tailrec_fold ( A ) ( B ) (sequence) (initial_value) (next_value)


 def   mk_Market (new_accounts : List ( Money ) ) (new_items : List ( Item ) ) : Market :=
    Market_ (new_accounts) (new_items)


 def   as_market (market : Market) : Market :=
    mk_Market (market.accounts) (market.items)


 private def   _change_owner (items : List ( Item ) ) (item_id : Index) (new_owner : Index) : List ( Item ) :=
    match (get (items) (item_id) ) with
      | some (item) =>
        set (items) (item_id) (Item_ (new_owner) (item.price) (item.advertised) )
      | otherwise => items
    


 def   change_owner (market : Market) (item_id : Index) (new_owner : Index) : Market :=
    mk_Market (market.accounts) (_change_owner (market.items) (item_id) (new_owner) )


 private def   _change_price (items : List ( Item ) ) (item_id : Index) (new_price : Money) : List ( Item ) :=
    match (get (items) (item_id) ) with
      | some (item) =>
        set (items) (item_id) (Item_ (item.owner) (new_price) (item.advertised) )
      | otherwise => items
    


 def   change_price (market : Market) (item_id : Index) (new_price : Money) : Market :=
    mk_Market (market.accounts) (_change_price (market.items) (item_id) (new_price) )


 private def   _advertise (items : List ( Item ) ) (item_id : Index) : List ( Item ) :=
    match (get (items) (item_id) ) with
      | some (item) =>
        set (items) (item_id) (Item_ (item.owner) (item.price) (true) )
      | otherwise => items
    


 def   advertise (market : Market) (item_id : Index) : Market :=
    mk_Market (market.accounts) (_advertise (market.items) (item_id) )


 private def   _remove_ad (items : List ( Item ) ) (item_id : Index) : List ( Item ) :=
    match (get (items) (item_id) ) with
      | some (item) =>
        set (items) (item_id) (Item_ (item.owner) (item.price) (false) )
      | otherwise => items
    


 def   remove_ad (market : Market) (item_id : Index) : Market :=
    mk_Market (market.accounts) (_remove_ad (market.items) (item_id) )


private def   _transfer_with_balances (accounts : List ( Money ) ) (origin : Index) (target : Index)
       (amount : Money) (origin_balance : Money) (target_balance : Money) : List ( Money ) :=
    set (set (accounts) (origin) (origin_balance - amount) ) (target) (target_balance + amount)


private def   _transfer_with (accounts : List ( Money ) ) (origin : Index) (target : Index) (amount : Money)
       (origin_balance : Money) : List ( Money ) :=
    match (get (accounts) (target) ) with
      | some (target_balance) =>
        _transfer_with_balances (accounts) (origin) (target) (amount) (origin_balance) (target_balance)
      | otherwise => accounts
    


private def   _transfer (accounts : List ( Money ) ) (origin : Index) (target : Index) (amount : Money)
       : List ( Money ) :=
    match (get (accounts) (origin) ) with
      | some (origin_balance) =>
        _transfer_with (accounts) (origin) (target) (amount) (origin_balance)
      | none => accounts
    


 def   sell (market : Market) (item_id : Index) (buyer : Index) : Market :=
    match (get (market.items) (item_id) ) with
      | some (item) =>
        mk_Market (
          _transfer (market.accounts) (buyer) (item.owner) (item.price) ) (
          set (market.items) (item_id) (Item_ (buyer) (item.price) (false) )
        )
      | otherwise =>
        market
    


 private def   _sum_pair (a : Money) (b : Money) : Money :=
    a + b


 def   assets (market : Market) : Money :=
    fold ( Money ) ( Money ) (market.accounts) (0) (_sum_pair)


  theorem
    lemma_set_keeps_length_1 (A : Type) (index : Index) (element : A) :
      ((Nil).set (index) (element) ).length = 0 :=
      by constructor

  theorem
    lemma_set_keeps_length_2 (A : Type) (head : A) (tail : List (A) ) (element : A) :
      ((head :: tail).set (0) (head)).length = ((element :: tail).set (0) (element)).length :=
      by constructor

  theorem
    set_keeps_length (A : Type) (list : List (A)) (index : Index) (element : A) :
      (list.set (index) (element) ).length = list.length :=
    match list with
      | Nil => lemma_set_keeps_length_1 (A) (index) (element)
      | (head) :: (tail) =>
        match index with
          | 0 => lemma_set_keeps_length_2 (A) (head) (tail) (element)
          | k + 1 => sorry

  theorem
    conservation_of_items_after_sell_operation (market : Market) (item_id : Index) (buyer : Index) :
       (sell (market) (item_id) (buyer) ).items.length = market.items.length :=
    sorry

  theorem
    lemma_fold (accounts : List (Money) ) (items : List (Item) ) (item_id : Index) (buyer : Index) :
     fold (Money) (Money) ( (sell (Market_ (accounts) (items)) (item_id) (buyer) ).accounts) (0) (_sum_pair) =
       fold (Money) (Money) (accounts) (0) (_sum_pair) :=
         sorry

  theorem
    conservation_of_money_after_sell_operation (market : Market) (item_id : Index) (buyer : Index) :
      assets (sell (market) (item_id) (buyer) ) = assets (market) :=
    lemma_fold (market.accounts) (market.items) (item_id) (buyer)

end MarketMod

open MarketMod
