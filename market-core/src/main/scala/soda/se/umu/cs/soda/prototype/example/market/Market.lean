/-
directive scala
object Succ_ {
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}
-/

notation head "+:" tail => (head) :: (tail)
notation "Succ_" => Nat.succ
notation "Int" => Nat

/-
directive coq
Notation "head '+:' tail" := (cons (head) (tail) ) (at level 99).
Notation "'Succ_'" := S (at level 99).
Notation "'Int'" := nat (at level 99).
-/

notation "Money" => Int

notation "Nat" => Int

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

class MyList

where
  mk ::
    bit : Bool
  deriving DecidableEq

namespace MyList


/-foldl
 (fold left)
-/

private def   _tailrec_foldl ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
       (next : B -> A -> B) : B :=
    match sequence with
      | List.nil => current
      | (head) +: (tail) =>
        _tailrec_foldl ( A ) ( B ) (tail) (next (current) (head) ) (next)
    


def   foldl ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (initial : B)
       (next : B -> A -> B) : B :=
    _tailrec_foldl ( A ) ( B ) (sequence) (initial) (next)


/- length
-/

 def   length_fl ( A : Type ) (list : List ( A ) ) : Nat :=
    foldl ( A ) ( Nat ) (list) (0) (
      fun (accum : Nat) =>
        fun (_elem : A) => accum + 1
    )


  theorem
    len_fl_accum (A : Type) (list : List (A) )
       : forall (accum : Nat) ,
        _tailrec_foldl (A) (Nat) (list) (accum) (fun (accum : Nat) => fun (elem : A) => accum + 1) =
           _tailrec_foldl (A) (Nat) (list) (0) (fun (accum : Nat) => fun (elem : A) => accum + 1) + accum := by
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

 private def   _tailrec_length ( A : Type ) (list : List ( A ) ) (accum : Nat) : Nat :=
    match list with
      | List.nil => accum
      | (_head) :: (tail) =>
        _tailrec_length ( A ) (tail) (accum + 1)
    


  theorem
    len_tr_accum (A : Type) (list : List (A) )
      : forall (accum : Nat) ,
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

 def   length_tr ( A : Type ) (list : List ( A ) ) : Nat :=
    _tailrec_length ( A ) (list) (0)


 def   length_def ( A : Type ) (list : List ( A ) ) : Nat :=
    match list with
      | List.nil => 0
      | (_head) :: (tail) => length_def ( A ) (tail) + 1
    


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

 def   length ( A : Type ) (list : List ( A ) ) : Nat :=
    length_fl ( A ) (list)


/- reverse
-/

 private def   _tailrec_reverse ( A : Type ) (list : List ( A ) ) (accum : List ( A ) ) : List ( A ) :=
    match list with
      | List.nil => accum
      | (head) :: (tail) => _tailrec_reverse ( A ) (tail) ( (head) :: (accum) )
    


 def   reverse_tr ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    _tailrec_reverse ( A ) (list) (List.nil)


 def   reverse_fl ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    foldl ( A ) ( List ( A )  ) (list) (List.nil) (
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
        rewrite [_tailrec_foldl, _tailrec_reverse]
        rfl
      | cons head tail ih =>
        intro other
        rewrite [_tailrec_foldl, _tailrec_reverse]
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

 def   reverse ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    reverse_fl ( A ) (list)


/- map
-/

private def   _tailrec_map_rev ( A : Type ) ( B : Type ) (list : List ( A ) ) (func : A -> B) (accum : List ( B ) )
       : List ( B ) :=
    match list with
      | List.nil => accum
      | (head) :: (tail) =>
          _tailrec_map_rev ( A ) ( B ) (tail) (func) ( (func (head) ) :: (accum) )
    


 def   map ( A : Type ) ( B : Type ) (list : List ( A ) ) (func : A -> B) : List ( B ) :=
    reverse_tr ( B ) (_tailrec_map_rev ( A ) ( B ) (list) (func) (List.nil) )


 def   map_def ( A : Type ) ( B : Type ) (list : List ( A ) ) (func : A -> B ) : List ( B ) :=
    match list with
      | List.nil => List.nil
      | (head) :: (tail) => (func (head) ) :: (map_def ( A ) ( B ) (tail) (func) )
    


/- concat
-/

 private def   _tailrec_concat ( A : Type ) (rev_first : List ( A ) ) (second : List ( A ) ) : List ( A ) :=
    match rev_first with
      | List.nil => second
      | (head) :: (tail) => _tailrec_concat ( A ) (tail) ( (head) :: (second) )
    


 def   concat ( A : Type ) (first : List ( A ) ) (second : List ( A ) ) : List ( A ) :=
    _tailrec_concat ( A ) (reverse ( A ) (first) ) (second)


 def   monus1 (index : Nat) : Nat :=
    match index with
      | 0 => 0
      | Succ_ (k) => k
    


  theorem
    monus1_succ
      : forall (index : Nat),
        monus1 (Nat.succ (index)) = index := by
    intro idx
    rewrite [monus1]
    simp

 private def   _tailrec_get ( A : Type ) (list : List ( A ) ) (index : Nat) : Option ( A ) :=
    match list with
      | List.nil => Option.none
      | (head) :: (tail) =>
        if index == 0
        then Option.some (head)
        else _tailrec_get ( A ) (tail) (monus1 (index) )
    


 def   get ( A : Type ) (list : List ( A ) ) (index : Nat) : Option ( A ) :=
    _tailrec_get ( A ) (list) (index)


private def   _tailrec_set ( A : Type ) (list : List ( A ) ) (accum : List ( A ) ) (index : Nat)
       (element : A) : List ( A ) :=
    match list with
      | List.nil => reverse ( A ) (accum)
      | (head) :: (tail) =>
        if index == 0
        then concat ( A ) (reverse ( A ) (accum) ) ( (element) :: (tail) )
        else _tailrec_set ( A ) (tail) ( (head) :: (accum) ) (monus1 (index) ) (element)
    


 def   set_tr ( A : Type ) (list : List ( A ) ) (index : Nat) (element : A) : List ( A ) :=
    _tailrec_set ( A ) (list) (List.nil) (index) (element)


 def   set_def ( A : Type ) (list : List ( A ) ) (index : Nat) (element : A) : List ( A ) :=
    match list with
      | List.nil => List.nil
      | (_head) :: (tail) =>
        if index == 0
        then (element) :: (tail)
        else (element) :: (set_def ( A ) (tail) (monus1 (index) ) (element) )
    


  theorem
    len_set (A : Type) (list : List (A)) (element : A)
      : forall (index : Nat),
        length_def (A) (set_def (A) (list) (index) (element) ) = length_def (A) (list) := by
    induction list with
    | nil =>
      intro idx
      rewrite [set_def, length_def]
      rfl
    | cons head tail ih =>
       intro idx
       rewrite [set_def, length_def]
       cases idx with
       | zero =>
         rewrite [monus1]
         rewrite [Nat.zero_eq]
         rfl
       | succ k =>
         rewrite [monus1]
         simp
         rewrite [length_def]
         rewrite [ih]
         rfl

 def   set ( A : Type ) (list : List ( A ) ) (index : Nat) (element : A) : List ( A ) :=
    set_tr ( A ) (list) (index) (element)


end MyList

notation "MyList_" => MyList.mk

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
      | _otherwise => items
    


 def   advertise (market : Market) (item_id : Nat) : Market :=
    Market.mk (market.accounts) (_advertise (market.items) (item_id) )


 private def   _remove_ad (items : List ( Item ) ) (item_id : Nat) : List ( Item ) :=
    match (_mm.get ( Item ) (items) (item_id) ) with
      | Option.some (item) =>
        _mm.set ( Item ) (items) (item_id) (Item_ (item.owner) (item.price) (false) )
      | _otherwise => items
    


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
      | _otherwise => accounts
    


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
      | _otherwise => market
    


 private def   _sum_pair (a : Money) (b : Money) : Money :=
    a + b


 def   assets (market : Market) : Money :=
    _mm.foldl ( Money ) ( Money ) (market.accounts) (0) (_sum_pair)


end MarketMod

notation "MarketMod_" => MarketMod.mk
