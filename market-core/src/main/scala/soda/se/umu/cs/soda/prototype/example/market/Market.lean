/- Prelude for Soda types. -/
notation "Boolean" => Bool
notation "None" => none
notation "Some" => some
notation "Nil" => []

/-
directive scala
type Nat = Int
-/

notation "Index" => Nat

notation "Money" => Int

class Item

where
  Item_ ::
    owner : Index
    price : Money
    advertised : Boolean
  deriving DecidableEq

namespace Item


end Item

notation "Item_" => Item.Item_

class Market

where
  Market_ ::
    accounts : List ( Money )
    items : List ( Item )
  deriving DecidableEq

namespace Market


end Market

notation "Market_" => Market.Market_

class MyList

where
  MyList_ ::
    bit : Boolean
  deriving DecidableEq

namespace MyList


/-foldl
 (fold left)
-/

private def   _tailrec_foldl ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
       (next_value : B -> A -> B) : B :=
    match sequence with
      | [] => current
      | (head) :: (tail) =>
        _tailrec_foldl ( A ) ( B ) (tail) (next_value (current) (head) ) (next_value)
    


def   foldl ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (initial_value : B)
       (next_value : B -> A -> B) : B :=
    _tailrec_foldl ( A ) ( B ) (sequence) (initial_value) (next_value)


/- length
-/

 def   length_fl ( A : Type ) (list : List ( A ) ) : Index :=
    foldl ( A ) ( Index ) (list) (0) (
      fun (accum : Index) =>
        fun (elem : A) => accum + 1
    )


  theorem
    len_fl_accum (A : Type) (list : List (A) )
       : forall (accum : Index) ,
        _tailrec_foldl (A) (Index) (list) (accum) (fun (accum : Index) => fun (elem : A) => accum + 1) =
           _tailrec_foldl (A) (Index) (list) (0) (fun (accum : Index) => fun (elem : A) => accum + 1) + accum := by
      induction list with
      | nil =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl, Nat.zero_add]
        rfl
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl]
        rewrite [ih (1)]
        rewrite [ih (n + 1)]
        rewrite [Nat.add_assoc]
        rewrite [Nat.add_comm 1]
        rfl

 private def   _tailrec_length ( A : Type ) (list : List ( A ) ) (accum : Index) : Index :=
    match list with
      | [] => accum
      | (head) :: (tail) =>
        _tailrec_length ( A ) (tail) (accum + 1)
    


  theorem
    len_tr_accum (A : Type) (list : List (A) )
      : forall (accum : Index) ,
        _tailrec_length (A) (list) (accum)  = _tailrec_length (A) (list) (0) + accum := by
      induction list with
      | nil =>
        intro n
        rewrite [_tailrec_length, _tailrec_length, Nat.zero_add]
        rfl
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_length, _tailrec_length]
        rewrite [ih (1)]
        rewrite [ih (n + 1)]
        rewrite [Nat.add_assoc]
        rewrite [Nat.add_comm 1]
        rfl

 def   length_tr ( A : Type ) (list : List ( A ) ) : Index :=
    _tailrec_length ( A ) (list) (0)


 def   length_def ( A : Type ) (list : List ( A ) ) : Index :=
    match list with
      | [] => 0
      | (head) :: (tail) => length_def ( A ) (tail) + 1
    


  theorem
  len_fl_eq_len_def (A : Type) (list : List (A))
      : length_fl (A) (list) = length_def (A) (list) := by
    rewrite [length_fl, foldl]
    induction list with
    | nil =>
      rewrite [_tailrec_foldl, length_def]
      rfl
    | cons head tail ih =>
      rewrite [_tailrec_foldl, len_fl_accum]
      rewrite [ih]
      rewrite [length_def]
      rfl

  theorem
    len_tr_eq_len_def
      : length_tr = length_def := by
    funext A list
    rewrite [length_tr]
    induction list with
    | nil =>
      rewrite [_tailrec_length, length_def]
      rfl
    | cons head tail ih =>
      rewrite [_tailrec_length, len_tr_accum]
      rewrite [ih]
      rewrite [length_def]
      rfl

 def   length ( A : Type ) (list : List ( A ) ) : Index :=
    length_fl ( A ) (list)


/- reverse
-/

 private def   _tailrec_reverse ( A : Type ) (list : List ( A ) ) (accum : List ( A ) ) : List ( A ) :=
    match list with
      | [] => accum
      | (head) :: (tail) => _tailrec_reverse ( A ) (tail) ( (head) :: (accum) )
    


 def   reverse_tr ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    _tailrec_reverse ( A ) (list) (Nil)


 def   reverse_fl ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    foldl ( A ) ( List ( A )  ) (list) (Nil) (
      fun (accum : List ( A ) ) =>
        fun (elem : A) =>
          (elem) :: (accum)
    )


  theorem
    rev_fl_accum (A : Type) (list : List (A))
      : forall (current: List (A) ),
        _tailrec_foldl (A) (List (A) ) (list) (current)
          (fun (accum : List (A) ) =>
            fun (elem : A) =>
               (elem) :: (accum)
          ) = _tailrec_reverse (A) (list) (current) := by
      induction list with
      | nil =>
        intro other
        rewrite [_tailrec_foldl,_tailrec_reverse]
        rfl
      | cons head tail ih =>
        intro other
        rewrite [_tailrec_foldl,_tailrec_reverse]
        rewrite [ih ((head) :: (other))]
        rfl

  theorem
    rev_tr_eq_rev_fl
      (A : Type) (list : List (A) )
        : reverse_fl (A) (list) = reverse_tr (A) (list) := by
    rewrite [reverse_fl, reverse_tr, foldl, rev_fl_accum]
    rfl

  theorem
    len_rev_accum (A : Type) (list : List (A))
      : forall (accum : List (A) ),
        length_def (A) (_tailrec_reverse (A) (list) (accum)) =
            length_def (A) (_tailrec_reverse (A) (list) ([])) + length_def (A) (accum) := by
      induction list with
      | nil =>
        intro other
        rewrite [_tailrec_reverse, _tailrec_reverse, length_def, Nat.zero_add]
        rfl
      | cons head tail ih =>
        intro other
        rewrite [_tailrec_reverse, _tailrec_reverse]
        rewrite [ih ((head) :: ([]))]
        rewrite [ih ((head) :: (other))]
        rewrite [length_def, length_def, length_def]
        rewrite [Nat.add_assoc, Nat.add_comm 1]
        rfl

  theorem
    rev_rev (A : Type) (list : List (A) )
      : reverse_tr (A) (reverse_tr (A) (list) ) = list := by
    rewrite [reverse_tr, reverse_tr]
    induction list with
    | nil =>
      rewrite [_tailrec_reverse, _tailrec_reverse]
      rfl
    | cons head tail ih =>
      rewrite [_tailrec_reverse]
      induction tail with
      | nil =>
        rewrite [_tailrec_reverse, _tailrec_reverse, _tailrec_reverse]
        rfl
      | cons hd tl ih2 =>
        sorry

 def   reverse ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    reverse_fl ( A ) (list)


/- map
-/

private def   _tailrec_map_rev ( A : Type ) ( B : Type ) (list : List ( A ) ) (func : A -> B) (accum : List ( B ) )
       : List ( B ) :=
    match list with
      | [] => accum
      | (head) :: (tail) =>
          _tailrec_map_rev ( A ) ( B ) (tail) (func) ( (func (head) ) :: (accum) )
    


 def   map ( A : Type ) ( B : Type ) (list : List ( A ) ) (func : A -> B) : List ( B ) :=
    reverse_tr ( B ) (_tailrec_map_rev ( A ) ( B ) (list) (func) (Nil) )


 def   map_def ( A : Type ) ( B : Type ) (list : List ( A ) ) (func : A -> B ) : List ( B ) :=
    match list with
      | [] => []
      | (head) :: (tail) => (func (head) ) :: (map_def ( A ) ( B ) (tail) (func) )
    


  theorem
    map_eq_map_def (A : Type) (B : Type) (list : List (A)) (func : A -> B)
      : map (A) (B) (list) (func) = map_def (A) (B) (list) (func) := by
    rewrite [map, reverse_tr]
    induction list with
    | nil =>
      rewrite [map_def, _tailrec_map_rev, _tailrec_reverse]
      rfl
    | cons head tail ih =>
      rewrite [map_def, _tailrec_map_rev]
      sorry

  theorem
    len_map (A : Type) (B : Type) (list : List (A) ) (func : A -> B)
      : length_tr (B) (map (A) (B) (list) (func) ) = length_tr (A) (list) := by
      rewrite [map_eq_map_def, len_tr_eq_len_def]
      induction list with
      | nil =>
        rewrite [map_def, length_def, length_def]
        rfl
      | cons head tail ih =>
        rewrite [map_def, length_def, length_def]
        rewrite [ih]
        rfl

/- concat
-/

 private def   _tailrec_concat ( A : Type ) (rev_first : List ( A ) ) (second : List ( A ) ) : List ( A ) :=
    match rev_first with
      | [] => second
      | (head) :: (tail) => _tailrec_concat ( A ) (tail) ( (head) :: (second) )
    


 def   concat ( A : Type ) (first : List ( A ) ) (second : List ( A ) ) : List ( A ) :=
    _tailrec_concat ( A ) (reverse ( A ) (first) ) (second)


 def   monus1 (index : Index) : Index :=
    if index <= 0
    then 0
    else index - 1


 private def   _tailrec_get ( A : Type ) (list : List ( A ) ) (index : Index) : Option ( A ) :=
    match list with
      | [] => none
      | (head) :: (tail) =>
        if index == 0
        then some (head)
        else _tailrec_get ( A ) (tail) (monus1 (index) )
    


 def   get ( A : Type ) (list : List ( A ) ) (index : Index) : Option ( A ) :=
    _tailrec_get ( A ) (list) (index)


private def   _tailrec_set ( A : Type ) (list : List ( A ) ) (accum : List ( A ) ) (index : Index)
       (element : A) : List ( A ) :=
    match list with
      | [] => reverse ( A ) (accum)
      | (head) :: (tail) =>
        if index == 0
        then concat ( A ) (reverse ( A ) (accum) ) ( (element) :: (tail) )
        else _tailrec_set ( A ) (tail) ( (head) :: (accum) ) (monus1 (index) ) (element)
    


 def   set ( A : Type ) (list : List ( A ) ) (index : Index) (element : A) : List ( A ) :=
    _tailrec_set ( A ) (list) (Nil) (index) (element)




  theorem
    len_set (A : Type) (list : List (A)) (index : Index) (element : A)
      : length (A) (set (A) (list) (index) (element) ) = length (A) (list) := by
        rewrite [set]
        sorry

end MyList

notation "MyList_" => MyList.MyList_

class MarketMod

where
  MarketMod_ ::
    bit : Boolean
  deriving DecidableEq

namespace MarketMod


 private def   _mm : MyList := MyList_ (true)


  notation "_mm.get" => MyList.get
  notation "_mm.set" => MyList.set
  notation "_mm.foldl" => MyList.foldl

 def   mk_Market (new_accounts : List ( Money ) ) (new_items : List ( Item ) ) : Market :=
    Market_ (new_accounts) (new_items)


 def   as_market (market : Market) : Market :=
    mk_Market (market.accounts) (market.items)


 private def   _advertise (items : List ( Item ) ) (item_id : Index) : List ( Item ) :=
    match (_mm.get ( Item ) (items) (item_id) ) with
      | some (item) =>
        _mm.set ( Item ) (items) (item_id) (Item_ (item.owner) (item.price) (true) )
      | otherwise => items
    


 def   advertise (market : Market) (item_id : Index) : Market :=
    mk_Market (market.accounts) (_advertise (market.items) (item_id) )


 private def   _remove_ad (items : List ( Item ) ) (item_id : Index) : List ( Item ) :=
    match (_mm.get ( Item ) (items) (item_id) ) with
      | some (item) =>
        _mm.set ( Item ) (items) (item_id) (Item_ (item.owner) (item.price) (false) )
      | otherwise => items
    


 def   remove_ad (market : Market) (item_id : Index) : Market :=
    mk_Market (market.accounts) (_remove_ad (market.items) (item_id) )


private def   _transfer_with_balances (accounts : List ( Money ) ) (origin : Index) (target : Index)
       (amount : Money) (origin_balance : Money) (target_balance : Money) : List ( Money ) :=
    _mm.set ( Money ) (_mm.set ( Money ) (accounts)
      (origin) (origin_balance - amount) ) (target) (target_balance + amount)


private def   _transfer_with (accounts : List ( Money ) ) (origin : Index) (target : Index) (amount : Money)
       (origin_balance : Money) : List ( Money ) :=
    match (_mm.get ( Money ) (accounts) (target) ) with
      | some (target_balance) =>
        _transfer_with_balances (accounts) (origin) (target)
          (amount) (origin_balance) (target_balance)
      | otherwise => accounts
    


private def   _transfer (accounts : List ( Money ) ) (origin : Index) (target : Index) (amount : Money)
       : List ( Money ) :=
    match (_mm.get ( Money ) (accounts) (origin) ) with
      | some (origin_balance) =>
        _transfer_with (accounts) (origin) (target) (amount) (origin_balance)
      | none => accounts
    


 def   sell (market : Market) (item_id : Index) (buyer : Index) : Market :=
    match (_mm.get ( Item ) (market.items) (item_id) ) with
      | some (item) =>
        mk_Market (
          _transfer (market.accounts) (buyer) (item.owner) (item.price) ) (
          _mm.set ( Item ) (market.items) (item_id) (Item_ (buyer) (item.price) (false) )
        )
      | otherwise =>
        market
    


 private def   _sum_pair (a : Money) (b : Money) : Money :=
    a + b


 def   assets (market : Market) : Money :=
    _mm.foldl ( Money ) ( Money ) (market.accounts) (0) (_sum_pair)


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
    lemma_foldl (accounts : List (Money) ) (items : List (Item) ) (item_id : Index) (buyer :
    Index) :
     _mm.foldl (Money) (Money) ( (sell (Market_ (accounts) (items)) (item_id) (buyer) ).accounts)
     (0) (_sum_pair) =
       _mm.foldl (Money) (Money) (accounts) (0) (_sum_pair) :=
         sorry

  theorem
    conservation_of_money_after_sell_operation (market : Market) (item_id : Index) (buyer : Index) :
      assets (sell (market) (item_id) (buyer) ) = assets (market) :=
    lemma_foldl (market.accounts) (market.items) (item_id) (buyer)

end MarketMod

notation "MarketMod_" => MarketMod.MarketMod_
