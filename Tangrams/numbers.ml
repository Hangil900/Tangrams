(** Some abstract algebra for you *********************************************)

module type Quotient = sig
  type number

  val ( === )  : number -> number -> bool
end

module QuotientProperties (Q : Quotient) = struct
  open Q
  let symmetric  a b   = if a === b then b === a else true;;
  let transitive a b c = if a === b &&   b === c then a === c else true;;
  let reflexive  a     = a === a;;
end

module type Group = sig
  include Quotient

  val zero   : number
  val ( +  ) : number -> number -> number
  val ( ~- ) : number -> number
end

module GroupProperties (G : Group) = struct
  open G

  include QuotientProperties (G)
  let commutative a b   = a + b === b + a
  let associative a b c = a + (b + c) === (a + b) + c
  let identity    a     = a + zero === a
  let inverses    a     = a + (~- a) === zero
end

module type Ring = sig
  include Group

  val one   : number
  val ( * ) : number -> number -> number
end

module RingProperties (R : Ring) = struct
  open R

  include GroupProperties (R)
  let times_associative  a b c = (a * b) * c === a * (b * c)
  let times_distributive a b c = a * (b + c) === a * b + a * c
  let times_identity     a     = a * one === a
  let times_commutative  a b   = a * b === b * a
end

module type Field = sig
  include Ring

  (* return the multiplicative inverse of the argument.  Behavior is undefined
   * if the argument is zero.
   *)
  val inv : number -> number
end

module FieldProperties (F : Field) = struct
  open F
  
  include RingProperties (F)
  let times_inverse a = if a === zero then true else a * inv a === one
end

module type OrderedRing = sig
  include Ring

  val is_non_neg : number -> bool
end

module OrderedRingProperties (R : OrderedRing) = struct
  open R

  let minus_one_negative     = not (is_non_neg ~-one)
  let squares_non_negative a = is_non_neg (a*a)

  let non_negative_times a b = if   is_non_neg a && is_non_neg b
                               then is_non_neg (a*b) else true

  let non_negative_plus  a b = if   is_non_neg a && is_non_neg b
                               then is_non_neg (a+b) else true
end

module type OrderedField = sig
  include Field
  include OrderedRing with type number := number
end

module OrderedFieldProperties (F : OrderedField) = struct
  include FieldProperties(F)
  include OrderedRingProperties(F)
end

(******************************************************************************)

module type NiceRing = sig
  include OrderedRing

  val float_of_number : number -> float
  val format          : Format.formatter -> number -> unit
  val roughly_half    : number -> number
end

module type NiceField = sig
  include OrderedField
  include (NiceRing with type number := number)
end

(** Exercise 1 ****************************************************************)

module type IntsType = NiceRing
module Ints : IntsType = struct
  type number = int
  let ( === ) = ( = )
  let zero  = 0
  let ( +  ) = ( + ) 
  let ( ~- ) = (~-)
  let one   = 1
  let ( * ) = ( * )
  let is_non_neg = fun x -> (x>=0)
  let float_of_number = fun x -> (float_of_int x)
  let format = fun formatter num -> Format.pp_print_int formatter num
  let roughly_half x = x/2  
end

module type IntegersType = NiceRing
module Integers : IntegersType = struct
  type number = Big_int.big_int
  let ( === )  = Big_int.eq_big_int
  let zero = Big_int.zero_big_int
  let ( + ) = Big_int.add_big_int
  let ( ~- ) = Big_int.minus_big_int
  let one = Big_int.unit_big_int
  let ( * ) = Big_int.mult_big_int
  let is_non_neg = fun x -> (Big_int.ge_big_int x zero)
  let float_of_number = Big_int.float_of_big_int
  let format = fun formatter num ->  
    Format.pp_print_string formatter (Big_int.string_of_big_int num)
  let roughly_half x = Big_int.div_big_int x (one + one)
end

module type FloatsType = NiceField
module Floats : FloatsType  = struct
  type number = float
  let ( === ) = fun x y -> (abs_float(x -. y) <= 10. ** (-6.))
  let zero  = 0.
  let ( +  ) = ( +. )
  let ( ~- ) = ( ~-. )
  let one   = 1.
  let ( * ) = ( *. ) 
  let is_non_neg = fun (x:number) -> (x>=0.)
  let inv = fun x -> if x <> 0. then (1. /. x) else failwith "division by zero"
  let float_of_number = fun x -> x
  let format          = fun formatter num ->
    Format.pp_print_float formatter num
  let roughly_half x = x /. 2.
end

module type Root23Type = sig
  include NiceRing
  val sqrt2 : number
  val sqrt3 : number
  val sqrt6 : number
end
module Root23 : Root23Type = struct
  module I = Integers
  (*********************** helpers *****************************)
  let ( == ) = I.( === ) 
  let ( ++ ) = I.( + )
  let ( ** ) = I.( * )
  let two = I.(+) I.one I.one 
  let three = I.(+) two I.one 
  let six = I.( * ) two three 
  type number = I.number * I.number * I.number * I.number
  let sqrt2 = (I.zero, I.one, I.zero, I.zero)
  let sqrt3 = (I.zero, I.zero, I.one, I.zero)
  let sqrt6 = (I.zero, I.zero, I.zero, I.one)
  (****************** actual things ******************************)
  let ( === ) = fun (a1,a2,a3,a4) (b1,b2,b3,b4) ->
    (a1 == b1) && (a2 == b2) && (a3 == b3) && (a4 == b4)
  let zero = (I.zero, I.zero, I.zero, I.zero)
  let ( + ) =  fun (a1,a2,a3,a4) (b1,b2,b3,b4) ->
    (a1 ++ b1, a2 ++ b2, a3 ++ b3, a4 ++ b4)
  let ( ~- ) = fun (a1,a2,a3,a4) -> (I.(~-) a1, I.(~-) a2, I.(~-) a3, I.(~-) a4)
  let one  = (I.one, I.zero, I.zero, I.zero) 
  let ( * ) =  fun (a1,a2,a3,a6) (b1,b2,b3,b6) -> 
    (* all the constant terms *)
    let x1 = (a1 ** b1) ++ (two ** (a2 ** b2)) ++ 
      (three ** (a3 ** b3)) ++ (six ** (a6 ** b6)) in
    (* all the sqrt2 terms *)
    let x2 = (a1 ** b2) ++ (a2 ** b1) ++ 
      (three ** a3 ** b6)  ++ (three ** a6 ** b3) in
    (* all the sqrt3 terms *)
    let x3 = (a1 ** b3) ++ (a3 ** b1) 
      ++ (two ** a2 ** b6) ++ (two ** a6 ** b2) in
    (* all the sqrt6 terms *)
    let x6 = (a1 ** b6) ++ (a6 ** b1) ++ (a2 ** b3) ++ (a3 ** b2) in
    (x1, x2, x3, x6)
  let is_non_neg = fun (a, b, c, d) -> 
    let ( ~- ) = I.( ~- ) in 
    let ( - ) x y = x ++ (~- y) in 
    (*returne true if a+(sqrt2*b) is non-negative *)
    let helper a b = 
      if I.is_non_neg a then 
        if I.is_non_neg b then true
        else I.is_non_neg ((a**a) - (two**b**b))
      else 
        if I.is_non_neg b then I.is_non_neg ((two**b**b) - (a**a))
        else false in
    (* (a + b * sqrt2) + (c + d * sqrt2) * sqrt3 *)
    let first = helper a b in
    let second = helper c d in 
    let constant = a**a ++ two**b**b - three**c**c - six**d**d in
    let root2 = two**a**b - six**c**d in
    if first then
      if second then true
      else helper constant root2
    else 
      if second 
      then 
        helper (~-constant) (~-root2)
      else false     
  let float_of_number =  fun (a1, a2, a3, a4) -> 
    (* simply calculates a + b*sqrt2 + c*sqrt3 + d*sqrt6 *)
    (I.float_of_number a1) +. (sqrt(2.) *. (I.float_of_number a2))
    +. (sqrt(3.) *. (I.float_of_number a3)) 
      +. (sqrt(6.) *. (I.float_of_number a4))
  let format = fun formatter (a1, a2, a3, a4) -> 
    let string_of_number x = string_of_float (I.float_of_number x) in
    let result = (string_of_number a1) ^ " + " ^ (string_of_number a2) ^ "_/2 + " ^ 
      (string_of_number a3) ^ "_/3 + " ^ (string_of_number a4) ^ "_/6" in
    Format.pp_print_string formatter result
  let roughly_half (a,b,c,d) = 
    (I.roughly_half a, I.roughly_half b, I.roughly_half c, I.roughly_half d)
end

module type FieldOfFractionsType = functor (R : NiceRing) -> sig
  include NiceField

  val from_numerator : R.number -> number

  (* refines NiceField.inv; raises an exception if the argument is zero. *)
  val inv : number -> number
end
module FieldOfFractions : FieldOfFractionsType = functor (R : NiceRing) -> struct
  (********* helper **********)
  let ( ** ) = R.( * )
  let ( ++ ) = R.( + )
  (********* actual stuff *******)
  type number = R.number * R.number (* numerator * denominator *) 
  let ( === ) = fun (a1,a2) (b1,b2) -> R.( === ) (a1 ** b2)  (a2 ** b1)
  let zero = (R.zero, R.one)
  let ( + ) =  fun (a1, a2) (b1, b2) -> 
    let numerator = (a1 ** b2) ++ (a2 ** b1) in
    let denominator = a2 ** b2 in
    (numerator, denominator)
  let ( ~- ) = fun (numerator, denominator) ->  (R.( ~-) numerator, denominator)
  let one = (R.one, R.one)
  let ( * ) =  fun (a1, a2) (b1, b2) ->
    let numerator = a1 ** b1 in
    let denominator = a2 ** b2 in
    (numerator, denominator)
  let is_non_neg = fun (numerator, denominator) -> 
    let is_non_neg_num = R.is_non_neg numerator in
    let is_non_neg_den = R.is_non_neg denominator in
    (is_non_neg_num && is_non_neg_den) 
    || ((not is_non_neg_num && not is_non_neg_den))
  let float_of_number = fun (num, denom) ->   
    let absol num = if R.is_non_neg num then num else (R.(~-) num) in 
    (* calculates how many times the number has to be halved 
     * so that it's smaller than max_float *)
    let rec until_small (numb: R.number) (k:int) = 
      if Pervasives.( < ) (R.float_of_number (absol numb))  max_float 
      then k 
      else until_small (R.roughly_half numb) (Pervasives.(+) k 1) in
    (* half the num k times*)
    let rec half_k num k = 
      if (Pervasives.(=) k 0) 
      then num 
      else half_k (R.roughly_half num) (k-1) in 
    let bigger x y = 
      if R.is_non_neg (R.(+) x (R.(~-) y)) then x else y in
    (* how many times num and denom have to be halved 
     * so neither of them is greater than max_float *)
    let number_halves = until_small (bigger (absol num) (absol denom)) 0 in
    let new_num = half_k num number_halves in
    let new_denom = half_k denom number_halves in
    (R.float_of_number new_num) /. (R.float_of_number new_denom)
  let format = fun formatter (x,y) ->
    let result =  (string_of_float (R.float_of_number x))^
      "/"
      ^(string_of_float (R.float_of_number y)) in
    Format.pp_print_string formatter result
  let from_numerator = fun (x:R.number) -> (x, R.one)
  let inv  = fun (num, denom) -> if (R.( === ) num R.zero) 
    then failwith "zero division" 
    else (denom, num)
  let roughly_half x = x * (inv (one + one))
end

module type RationalsType = NiceField
module Rationals : RationalsType = struct
  include FieldOfFractions(Integers) 
end

module type Rot15Type = sig
  include NiceField
  val cos45 : number
  val sin45 : number
  val cos30 : number
  val sin30 : number
end

module Rot15 : Rot15Type = struct
  include FieldOfFractions(Root23)
  (************ helper **************)
  let ( ++ ) = Root23.(+) 
  let sqrt2 = from_numerator(Root23.sqrt2)
  let sqrt3 = from_numerator(Root23.sqrt3)
  let half = inv(from_numerator((Root23.one ++ Root23.one))) 
  (********** trig values *************)
  let cos45 = sqrt2 * half
  let sin45 = cos45
  let cos30 = sqrt3 * half
  let sin30 = half
end

(** Exercise 2 ****************************************************************)

module QuotientUtils (Q : Quotient) = struct
  let (<>) x y =  not (Q.( === ) x y)
end

module GroupUtils (G : Group) = struct
  include QuotientUtils (G)
  let (-) x y = G.( +) x (G.( ~- ) y)
end

module RingUtils (R : Ring) = struct
  include GroupUtils (R)
  (* logrithmic approach to make the calculation faster *)
  let number_of_int n = 
    let ( + ) = R.(+) in
    let ( * ) = R.( * ) in
    let ( ~- ) = R.(~-) in
    let div_mod num = ((Pervasives.(/) num 10), (Pervasives.(mod) num 10)) in
    let rec from_int k = if k=0 then R.zero
      else R.one + (from_int (Pervasives.(-) k 1)) in
    let rec helper num = 
      if (Pervasives.(<) num 0) then ~-(helper (Pervasives.(~-) num))
      else if num = 0 then R.zero
      else 
        let (r, k) = div_mod num in
        (from_int 10) * (helper r) + (from_int k) in
    helper n
end

module FieldUtils (F : Field) = struct
  include RingUtils (F)
  let (/) x y = F.( * ) x (F.inv y)
end

module OrderedRingUtils (R : OrderedRing) = struct
  include RingUtils (R)
  let (>=) x y = R.is_non_neg (x-y)
  let (<=) x y = R.is_non_neg (y-x)    
  let (<)  x y = not (x >= y)
  let (>)  x y = not (x <= y) 
  

  (* return the smaller of x and y under the (<) ordering *)
  let min  x y = if x < y then x else y

  (* return the larger of x and y under the (<) ordering *)
  let max  x y = if x > y then x else y

  (* implement the Set.OrderedType.compare interface *)
  let compare x y = x - y
end

module OrderedFieldUtils (F : OrderedField) = struct
  include FieldUtils (F)
  include OrderedRingUtils (F)
end

(** Exercise 3 ****************************************************************)

(* TODO: this implementation of Rationals  is here so that the provided
 * interface for Reals compiles; it should be removed when you implement the
 * Rationals above
 *)

module type RealsType = sig
  include NiceField
  

  (* given a sequence f of rational approximations f(1) f(2) f(3) ... that
   * converges to a real number x, (create f) returns x.
   *
   * f is required to converge at a rate of 10^(-k).  That is, for all k,
   * |x - f(k)| < 10^(-k).  Behavior is completely unspecified if f does not
   * converge fast enough.
   *)
  val create   : (int -> Rationals.number) -> number

  (* approximate x k produces a rational approximations of x that is accurate
   * to within 10^(-k).  In other words, |x - (approximate x k)| < 10^(-k)
   *)
  val approximate : number -> int -> Rationals.number

  val pi : number
  val e  : number
end

module Reals : RealsType = struct
  module R = Rationals
  module P = Pervasives
  type number = int -> R.number
  include OrderedFieldUtils(R)
  (*********** helper functions for this module *******)
  let ( ** ) = R.( * )
  let ( ++ ) = R.( + )
  let power x k = 
    let rec helper x k acc = if (P.(<) k 0) then R.inv (helper x (P.(~-) k) acc)
      else if k = 0 then acc
      else helper x (pred k) (acc ** (number_of_int x)) in 
    helper x k R.one
  let rec ten_power k = power 10 k
  let inv_ten_power k =  R.inv (ten_power k) 
  (*************** Actual functions for this module ************)
  (* zero division is not handled 
   * because you can't figure out if the num will converge to zero *)
  let inv = fun num -> fun n ->
    (* inequality: abs(1/(x_k + 2*10^(-k))  - 1/(x_k -2*10^(-k))) <= 10^(-n) *)
    let rec hold_inequality num k n = 
      let abs rnumber = if R.is_non_neg rnumber 
        then rnumber else R.(~-) rnumber in
      let error = inv_ten_power k in
      let first_term = R.inv ((num k) ++ ((number_of_int 2) ** error)) in
      let second_term = R.inv ((num k) - ((number_of_int 2) ** error)) in
      let inequality = (abs (first_term - second_term)) < (inv_ten_power n) in
      if inequality then k else hold_inequality num (succ k) n in
    let how_deep = hold_inequality num n n in
    R.(inv) (num how_deep)  
  let ( === ) x y = 
    let rec equals n = 
      let diff = (x n) - (y n) in
      let limit = ((R.one ++ R.one) / (ten_power n)) in
      let abs x = if (R.is_non_neg x) then x else R.(~-) x in
      if (abs diff) < limit then equals (succ n)
      else false in
    equals 0
  (* (x+y)_n = x_(n+1)   +  y_(n+1) *) 
  let ( + ) = fun n1 n2 -> fun k -> 
    ((n1 (succ k)) ++ (n2 (succ k)))
  let ( ~- ) = fun num -> (fun x -> (R.(~-) (num x)))
  (* (xy)_n = x_(n+ky+1) * y_(n+kx+1) *)
  let ( * ) = fun x y -> fun n ->
    (* returns k such that (num + 1) < 10^k *)
    let rec ten_how_big num n k = 
      let limit = ten_power k in
      let compare_number  = (num n) ++ R.one in
      if compare_number < limit then k
      else ten_how_big num n (succ k) in 
    let x_depth = P.(+) (succ n)  (ten_how_big y 0 n) in 
    let y_depth = P.(+) (succ n) (ten_how_big x 0 n) in 
    R.( * ) (x x_depth) (y y_depth)  
  let is_non_neg = fun num -> 
    let rec check num n = if R.(+) (num n) (inv_ten_power n) < R.zero then false
      else if (num n) - (inv_ten_power n) >= R.zero then true
      else check num (succ n) in 
    check num 0
  let float_of_number = fun num -> R.float_of_number (num 10)
  let format = fun formatter num -> Format.pp_print_float formatter (float_of_number num)
  let create      = fun f -> f
  let approximate = fun num k -> (num k)
  let roughly_half x = fun n -> (R.roughly_half (x n))
  (************* values ***************)
  let zero = fun x -> R.zero 
  let one = fun x -> R.one
  (* spigot algorithm starting at index 1 *)
  let pi = fun n -> 
    let term (i:int) = 
      (* returns 1/(8i+x) *)
      let helper (x:int) = 
        R.inv (((number_of_int 8) ** (number_of_int i)) ++ (number_of_int x)) in
      (* 1/16^i *)
      let first = power 16 (P.(~-) i) in
      (* 4/(8i+1) *)
      let second = (number_of_int 4) ** (helper 1) in
      (* -2/(8i+4) *)
      let third = R.(~-) ((number_of_int 2) ** (helper 4)) in
      (* -1/(8i+5) *)
      let fourth = R.(~-) (helper 5) in
      (* -1/(8i+6) *) 
      let fifth = R.(~-) (helper 6) in 
      R.( * ) first (second ++ third ++ fourth ++ fifth) in
    (* sum the term from 0 to i*)
    let sum_terms i = 
      let rec helper i acc = if i = 0 then acc 
        else helper (pred i) (acc ++ (term i)) in
      helper i (term 0) in
     if (P.(<) n 0) then sum_terms 1 
     else sum_terms (succ n) 
  (* e_n = taylor series starting at index n + 11 *)
  let e = fun (n:int) -> 
    let factorial (n:int): R.number = 
      let rec helper n acc = 
        if (P.(=) n 0) 
        then acc else helper (pred n) (acc ** (number_of_int n)) in
      helper n R.one in
    let sum_to (k:int) = 
      let rec helper k acc = if (P.(=) k 0) then acc
      else helper (pred k) (acc ++ (R.inv (factorial k))) in 
      helper k R.one in
    if (P.(<) n 0) then sum_to 11
    else sum_to (P.(+) n 11)                                   
end

(******************************************************************************)

(*
** vim: ts=2 sw=2 ai et
*)
