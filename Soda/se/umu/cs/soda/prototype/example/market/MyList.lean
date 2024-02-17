

import Soda.se.umu.cs.soda.prototype.example.market.Basic

class IndexOption ( A : Type )

where
  mk ::
    current_index : Nat
    maybe_elem : Option ( A )
  deriving DecidableEq

namespace IndexOption


end IndexOption

notation "IndexOption_" => IndexOption.mk

class ChangeWindow ( A : Type )

where
  mk ::
    current_index : Nat
    target_index : Nat
    new_value : A
    rev_accum : List ( A )
  deriving DecidableEq

namespace ChangeWindow


end ChangeWindow

notation "ChangeWindow_" => ChangeWindow.mk

class MyList

where
  mk ::
    bit : Bool
  deriving DecidableEq

namespace MyList


/-`_tailrec_foldl` is a 'fold left' function for parameterized types.
 This definition of fold left is tail recursive.
-/

private def   _tailrec_foldl ( A : Type ) ( B : Type ) (list : List ( A ) ) (current : B)
       (next : B -> A -> B) : B :=
    match list with
      | List.nil => current
      | (head) :: (tail) =>
        _tailrec_foldl ( A ) ( B ) (tail) (next (current) (head) ) (next)
    


/-  `foldl` is a 'fold left' function for parameterized types that uses `_tailrec_foldl`.
-/

def   foldl ( A : Type ) ( B : Type ) (list : List ( A ) ) (initial : B)
       (next : B -> A -> B) : B :=
    _tailrec_foldl ( A ) ( B ) (list) (initial) (next)


/-`length` defined using fold left.
 This uses foldl, which is tail recursive.
-/

 def   length_fl ( A : Type ) (list : List ( A ) ) : Nat :=
    _tailrec_foldl ( A ) ( Nat ) (list) (0) (
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
    rewrite [length_fl]
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
    _tailrec_foldl ( A ) ( List ( A )  ) (list) (List.nil) (
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
    rewrite [reverse_fl, reverse_tr, rev_fl_accum]
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

 def   map_fl ( A : Type ) ( B : Type ) (list : List ( A ) ) (f : A -> B) : List ( B ) :=
    reverse_fl ( B ) (
      foldl ( A ) ( List ( B )  ) (list) (List.nil) (
        fun (accum : List ( B ) ) =>
          fun (elem : A) =>
            (f (elem) ) :: (accum)
      )
    )


 def   map_def ( A : Type ) ( B : Type ) (list : List ( A ) ) (func : A -> B ) : List ( B ) :=
    match list with
      | List.nil => List.nil
      | (head) :: (tail) => (func (head) ) :: (map_def ( A ) ( B ) (tail) (func) )
    


 def   map ( A : Type ) ( B : Type ) (list : List ( A ) ) (f : A -> B) : List ( B ) :=
    map_fl ( A ) ( B ) (list) (f)


/- concat
-/

 def   concat ( A : Type ) (first : List ( A ) ) (second : List ( A ) ) : List ( A ) :=
    _tailrec_foldl ( A ) ( List ( A )  ) (reverse_fl ( A ) (first) ) (second) (
      fun (accum : List ( A ) ) =>
        fun (elem : A) =>
          (elem) :: (accum)
    )


 def   monus1 (index : Nat) : Nat :=
    match index with
      | Succ_ (k) => k
      | _otherwise  => 0
    


  theorem
    monus1_succ
      : forall (index : Nat),
        monus1 (Nat.succ (index)) = index := by
    intro idx
    rewrite [monus1]
    simp

 def   get ( A : Type ) (list : List ( A ) ) (index : Nat) : Option ( A ) :=
    (foldl ( A ) ( IndexOption ( A )  ) (list) (
      IndexOption.mk (0) (Option.none) ) (
        fun (accum : IndexOption ( A ) ) =>
          fun (elem : A) =>
            if (accum.current_index == index)
            then IndexOption.mk (accum.current_index + 1) (Option.some (elem) )
            else IndexOption.mk (accum.current_index + 1) (accum.maybe_elem)
      )
    ).maybe_elem


private def   _replace_if_in_place ( A : Type ) (current_index : Nat) (target_index : Nat) (old_value : A) (
       new_value : A) : A :=
    if (current_index == target_index)
    then new_value
    else old_value


 private def   _apply_replacement ( A : Type ) (p : ChangeWindow ( A ) ) (element : A) : ChangeWindow ( A ) :=
    ChangeWindow.mk (p.current_index + 1) (p.target_index) (p.new_value) (
      (_replace_if_in_place ( A ) (p.current_index) (p.target_index) (element) (p.new_value)
      ) :: p.rev_accum
    )


 private def   _initial_window ( A : Type ) (index : Nat) (new_value : A) : ChangeWindow ( A ) :=
    ChangeWindow.mk (0) (index) (new_value) (List.nil)


 def   set_fl ( A : Type ) (list : List ( A ) ) (index : Nat) (new_value : A) : List ( A ) :=
    reverse_fl ( A ) (
      (foldl ( A ) ( ChangeWindow ( A )  ) (list) (_initial_window ( A ) (index) (new_value) ) (
        _apply_replacement ( A ) ) ).rev_accum
    )


 def   set_def ( A : Type ) (list : List ( A ) ) (index : Nat) (element : A) : List ( A ) :=
    match list with
      | List.nil => List.nil
      | (_head) :: (tail) =>
        if index == 0
        then (element) :: (tail)
        else (element) :: (set_def ( A ) (tail) (monus1 (index) ) (element) )
    


  theorem    len_set (A : Type) (list : List (A)) (element : A)
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
    set_fl ( A ) (list) (index) (element)


end MyList

notation "MyList_" => MyList.mk
