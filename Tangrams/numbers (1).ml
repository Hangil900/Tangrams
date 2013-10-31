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
  val roughly_half : number -> number
  val float_of_number : number -> float
  val format          : Format.formatter -> number -> unit
end

module type NiceField = sig
  include OrderedField
  include (NiceRing with type number := number)
end

(** Exercise 2 ****************************************************************)

module QuotientUtils (Q : Quotient) = struct
  let (<>) x y = not (Q.( === ) x y)
end

module GroupUtils (G : Group) = struct
  include QuotientUtils (G)
  let (-) x y = G.( + ) x (G.( ~- ) y)
end

module RingUtils (R : Ring) = struct
  include GroupUtils (R)
  let number_of_int n = 
    let open R in
    let two = (one + one) in
    let ten = two*two*two + two in
    let rec helper (acc:number) (n:int) (digit_pwr:int)= 
      if n = 0 then acc
      else
	let power (base:number) (ex:int) = 
	  let rec helper base ex acc = 
	    if ex = 0 then acc
	    else helper base (Pervasives.(-) ex 1) (base * acc) in
	  helper base (abs(ex)) one in
	let next = n mod 10 in
	let rest = Pervasives.(/) n 10 in
	(*Returns the digit as a R.number*)
	let rec find_digit dig acc = if dig = 0 then acc
	  else find_digit (Pervasives.(-) dig 1) (acc + one) in
	let next_dig = find_digit next zero in
	(* Multiply it by 10^digit_pwr to account for the continous mod*) 
	let next_num = next_dig * (power ten digit_pwr) in
	helper (acc + next_num) (rest) (Pervasives.(+) digit_pwr 1) in
    let num = helper zero (abs n) 0 in
    if Pervasives.(<) n 0 then -num else num
end

module FieldUtils (F : Field) = struct
  include RingUtils (F)
  let (/) x y = F.( * ) x (F.inv y)
end

module OrderedRingUtils (R : OrderedRing) = struct
  include RingUtils (R)
  let (<)  x y = not (R.is_non_neg (x - y))
  let (>)  x y = 
    if R.( === ) x y then false
    else not (x < y)
  let (<=) x y = not (x > y)
  let (>=) x y = not (x < y)

  (* return the smaller of x and y under the (<) ordering *)
  let min  x y = if x<y then x else y

  (* return the larger of x and y under the (<) ordering *)
  let max  x y = if x> y then x else y

  (* implement the Set.OrderedType.compare interface *)
  let compare x y = if x < y then -1
    else if x> y then 1
    else 0
end

module OrderedFieldUtils (F : OrderedField) = struct
  include FieldUtils (F)
  include OrderedRingUtils (F)
end

(** Exercise 1 ****************************************************************)

module type IntsType = NiceRing
(* TODO: module Ints : IntsType = ... *)
module Ints: IntsType =
struct
  type number = int
  let ( === ) : number->number->bool= fun num1 num2 -> (num1 = num2)
  let zero: number= 0
  let ( + ): number -> number -> number = Pervasives.(+)
  let ( ~- ): number->number = Pervasives.( ~- )
  let  one :number = 1
  let ( * ) : number -> number -> number = Pervasives.( * )
  let is_non_neg : number -> bool = fun num -> num >= 0
  let float_of_number : number -> float = float_of_int
  let format : Format.formatter -> number -> unit = fun formatter num ->
    Format.pp_print_int formatter num
  let roughly_half (num:number) = num / 2
end


module type IntegersType = NiceRing
(* TODO: module Integers : IntegersType = ... *)
module Integers: IntegersType = struct
  open Big_int
  type number = big_int
  let ( === ) : number->number->bool= eq_big_int
  let zero: number= zero_big_int
  let ( + ): number -> number -> number =
    fun num1 num2 -> add_big_int num1 num2
  let ( ~- ): number->number = fun num -> minus_big_int num
  let  one :number = unit_big_int
  let ( * ) : number -> number -> number = mult_big_int
  let is_non_neg : number -> bool = fun num ->(sign_big_int num) >=0
  let float_of_number : number -> float = float_of_big_int
  let format : Format.formatter -> number -> unit = fun formatter num ->
    Format.pp_print_string formatter (string_of_big_int num)
  let roughly_half (num:number) = div_big_int num (one+one)
end


module type FloatsType = NiceField
(* TODO: module Floats : FloatsType  = ... *)
module Floats: FloatsType = struct
  type number = float
  let inv : number -> number = fun num -> if num <> 0. then 1. /. num
    else failwith "Can't divide by 0"
  let ( === ) : number->number->bool= 
    fun num1 num2 -> (abs_float(num1 -. num2) <= (10. ** -6.))
  let zero: number= 0.
  let ( + ): number -> number -> number = ( +. )
  let ( ~- ): number->number = ( ~-. )
  let  one :number = 1.
  let ( * ) : number -> number -> number = ( *. )
  let is_non_neg : number -> bool = fun num -> (num >= 0.)
  let float_of_number : number -> float = fun num -> num
  let roughly_half (num:number) = num /. 2.
  let format : Format.formatter -> number -> unit =fun formatter num ->
    Format.pp_print_float formatter num
end

module type Root23Type = sig
  include NiceRing
  val sqrt2 : number
  val sqrt3 : number
  val sqrt6 : number
end
(* TODO: module Root23 : Root23Type = ... *)
module Root23: Root23Type = struct
  module I = Integers
  type number = I.number*I.number*I.number*I.number
  let sqrt2:number = (I.zero, I.one, I.zero, I.zero)
  let sqrt3:number = (I.zero, I.zero, I.one, I.zero)
  let sqrt6:number = (I.zero, I.zero, I.zero, I.one)
  let ( === ) : number->number->bool= 
    let open I in
    fun num num2 -> match (num, num2) with
    | ((a, b, c, d), (e, f, g, h)) -> 
      ((a === e) && (b === f) &&(c === g) && (d === h))
  let zero: number= (I.zero, I.zero, I.zero, I.zero)
  let ( + ): number -> number -> number = 
    let open I in
    fun num num2 ->match (num, num2) with
    | ((a, b, c, d), (e, f, g, h)) -> 
      (a+ e, b + f, c + g, d + h)
  let ( ~- ): number->number = fun num -> match num with
    |(a,b,c,d) -> (I.(~-) a,I.(~-) b,I.(~-) c,I.(~-) d)
  let  one :number = (I.one, I.zero, I.zero, I.zero)
  let ( * ) : number -> number -> number = fun n n2 -> match (n,n2) with
    | ( (a,b,c,d) , (a2,b2,c2,d2) ) -> begin
      let one = I.one in
      let two = I.(+) one one in
      let thr = I.(+) one two in
      let six = I.(+) thr thr in
      let ( * ) = I.( * ) in
      let num1 = (a*a2, b*a2, c*a2, d*a2) in
      let num2 = (two*b*b2, a*b2, two*d*b2, c*b2) in
      let num3 = (thr*c*c2, thr*d*c2, a*c2, b*c2) in
      let num4 = (six*d*d2, thr*c*d2, two*b*d2, a*d2) in
      num1 + num2 + num3 + num4
    end
  let is_non_neg : number -> bool = fun num ->
    let two = I.(+) I.one I.one in
    let thr = I.(+) I.one two in
    (**Returns true if (a+b*sqrt2) is positive else false**)
    let is_pos (a:I.number) (b:I.number) = 
      let open I in
      match (is_non_neg a, is_non_neg b) with
      | (true, true) -> true
      | (false, false) -> false
      | (first_true, _) -> 
	let sqDiff = ((a * a) + (~- (two * b* b))) in
	if sqDiff === zero then true
	else if first_true then is_non_neg sqDiff
	else not (is_non_neg sqDiff) in
    match num with
    | (e,f,g,h) -> match ((is_pos e f), (is_pos g h)) with
      | (true, true) -> true
      | (false, false) -> false
      | (first,second) -> begin
	let ( ** ) = I.( * ) in
	let n1 = (e,f, I.zero, I.zero) * (e,f, I.zero, I.zero) in
	let n2 = 
	  let x = ( thr ** g, thr ** h, I.zero, I.zero) in
	   x * (g,h, I.zero, I.zero) in
	let fnl = if first then n1 + (~-n2)
	  else (~-n1) + n2 in
	(match fnl with
	| (a2,b2,_,_ ) -> is_pos a2 b2)
      end			 
  let float_of_number : number -> float = fun num -> match num with
    | (a,b,c,d) -> 
      let open I in
      let n1= (float_of_number a) in
      let n2 = ((float_of_number b) *. sqrt 2.) in
      let n3= ((float_of_number c) *. sqrt 3.) in
      let n4 = ((float_of_number d) *. sqrt 6.) in
      n1 +. n2 +. n3 +. n4
  let format : Format.formatter -> number -> unit =
    fun formatter num -> match num with
    |(a,b,c,d) -> I.format formatter a;
      Format.pp_print_string formatter ("+ ");
      I.format formatter b;
      Format.pp_print_string formatter ("sqrt2 + ");
      I.format formatter c;
      Format.pp_print_string formatter ("sqrt3 + ");
      I.format formatter d;
      Format.pp_print_string formatter ("sqrt6")
  (*Shouldn't be used directly to work with numbers. Only when approx floats*)
  let roughly_half (num:number) = match num with
    | (a,b,c,d) ->
      let open I in
      (roughly_half a,roughly_half b,roughly_half c, roughly_half d)
end

module type FieldOfFractionsType = functor (R : NiceRing) -> sig
  include NiceField

  val from_numerator : R.number -> number

  (* refines NiceField.inv; raises an exception if the argument is zero. *)
  val inv : number -> number
end
(* TODO: module FieldOfFractions : FieldOfFractionsType = ...*) 
module FieldOfFractions : FieldOfFractionsType = functor (R: NiceRing) -> 
struct
  type number = R.number*R.number
  let inv: number -> number = 
    let open R in
    fun n -> match n with
    | (a,b) -> if a===zero then failwith "No inv for Zero"
      else if is_non_neg a then (b,a)
      else ( ~-b, ~-a)
  let from_numerator (n:R.number):number = (n, R.one)
  let ( === ) : number->number->bool= 
    let open R in
    fun (a,b) (c,d) -> (a * d) === (b * c)
  let zero: number = (R.zero, R.one)
  let ( + ) (n:number) (n2:number): number = match (n,n2) with
    | ((a,b),(c,d)) -> 
      let open R in
      let nume = (a * d)+ (c * b) in
      let denom = b* d in
      (nume, denom)
  let ( ~- ) (n:number):number = match n with
    |(a,b) -> (R.( ~- ) a, b)
  let  one :number = (R.one, R.one)
  let ( * ) (n1:number) (n2:number): number = match (n1,n2) with
    |((a,b),(c,d)) ->  ((R.( * ) a c), (R.( * ) b d))
  let is_non_neg (n:number):bool = match n with
    |(a,_) -> R.is_non_neg a
  let roughly_half (n:number):number = match n with
    | (num, den) -> (R.roughly_half num, R.roughly_half den)
  let float_of_number (n:number):float = match n with
    |(num, den) -> 
      let ab num:float = if num >= 0. then num else Pervasives.(~-.) num in
      (*If the number is too big, keep reducing by roughly half*)
      let rec reduce (n1:R.number) (n2:R.number) = 
	let open Pervasives in
	if ( ab(R.float_of_number n1) > max_float || 
	       ab(R.float_of_number n2) > max_float) 
	then reduce (R.roughly_half n1) (R.roughly_half n2)
	else (n1, n2) in
      let (new_num, new_den) = reduce num den in
      (R.float_of_number new_num) /. (R.float_of_number new_den)
  let format : Format.formatter -> number -> unit = 
    fun f num -> match num with
    | (n,d) -> R.format f n;
      Format.pp_print_string f "/ ";
      R.format f d
end

module type RationalsType = NiceField
(* TODO: module Rationals : RationalsType = ... *)
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
(* TODO: module Rot15 : Rot15Type = ... *)
module Rot15: Rot15Type = struct
  include FieldOfFractions(Root23)
  module Ro = Root23
  let two = Ro.(+) Ro.one Ro.one
  let cos45:number =
    let top = from_numerator (Ro.sqrt2) in
    let btm =  inv(from_numerator two) in
    ( * ) top btm
  let sin45: number = cos45
  let cos30:number = 
    let top = from_numerator (Ro.sqrt3) in
    let btm = inv(from_numerator two) in
    ( * ) top btm
  let sin30:number = 
    inv(from_numerator two)
end

(** Exercise 3 ****************************************************************)

module type RealsType = sig
  include NiceField

  (* given a sequence f of rational approximations f(1) f(2) f(3) ... that
   * converges to a real number x, (create f) returns x.
   *
   * f is required to converge at a rate of 10^(-k).  That is, for all k,
   * |x - f(k)| < 10^(-k).  Behavior is completely unspecified if f does not
   * converge fast enough.
   *)
  val create      : (int -> Rationals.number) -> number 
  (* approximate x k produces a rational approximations of x that is accurate
   * to within 10^(-k).  In other words, |x - (approximate x k)| < 10^(-k)
   *)
  val approximate : number -> int -> Rationals.number

  val pi : number
  val e  : number
end
(* TODO: module Reals : RealsType = ... *)

module Reals: RealsType = struct
  include OrderedFieldUtils(Rationals)
  module R= Rationals
  module P = Pervasives
  type number = int-> R.number
(* Should not be used, but is required by the NiceRing interface*)
  let roughly_half (n:number):number = fun k -> R.roughly_half (n k)
  let  power (base:R.number) (ex:int): R.number = 
    let rec helper base ex acc = 
      if ex = 0 then acc
      else helper base (pred ex) (R.( * ) base acc) in
    let num = helper base (abs(ex)) R.one in
    if P.(>=) ex 0 then num else (R.inv num)
  let two:R.number = number_of_int 2
  let ten: R.number = number_of_int 10 
  let create (f:int->R.number): number= f
  let approximate (n:number) (k:int) :R.number = n k
  let ( === ) (n:number) (n2:number):bool = 
    let rec equals k = 
      let diff = ((n k) - (n2 k)) in
      let invar = (two / (power ten k)) in
      let abs_diff = if R.is_non_neg diff then diff
	else R.(~-) diff in
      if abs_diff < invar then equals (succ k)
      else false in
    equals 0
  let zero : number = fun k -> R.zero
  let ( + ) (n:number) (n2:number):number = 
    let open P in
    fun k-> R.( + ) (n (1+k)) (n2 (1+k))
  let ( ~- ) (n:number):number = fun k -> R.( ~- ) (n k)
  let one : number = fun k -> R.one
  let ( * ) (n:number) (n2:number) : number =
    fun k -> 
      (* Calculates extra depth we have to go to maintain invariant*)
      let rec find_extra (org:int) (ext:int) (num:number) = 
	let (bound:R.number) = (power ten ext) in
	let open R in
	if ((num org)+ one) < bound then ext
	else find_extra org (succ ext) num in
      let n_extra = find_extra k 0 n2 in
      let n2_extra = find_extra k 0 n in
      let open P in
      let n_depth = (k + (n_extra + 1)) in
      let n2_depth = (k + (n2_extra + 1)) in
      R.( * ) (n n_depth) (n2 n2_depth)
  let is_non_neg (n:number): bool = 
    let open R in
    let minus = P.(~-) in
    let rec non_neg k = if (n k) + (power ten (minus k)) < R.zero then false
      else if (n k) - (power ten (minus k)) >= R.zero then true
      else non_neg (succ k) in
    non_neg 0
  let float_of_number : number -> float = fun num ->
    (* Float precision is 2^64. Thus we can do 10^64. 
       But Mike George just said use 10 for simplicity *)
    R.float_of_number (num 10)
  let  inv (n:number):number = 
    let open R in
    fun k -> 
      let rec invariant num extra orig =
	let abs num = if is_non_neg num then num else ~-num in
	let eps = inv (power ten extra) in
	let fst_val = inv ((num extra) + ((number_of_int 2)* eps)) in
	let snd_val = inv ((num extra) + ((number_of_int 2)* eps)) in
	let hold_inv = (abs(fst_val - snd_val)) < inv (power ten orig) in
	if hold_inv then extra 
	else invariant num (succ extra) orig in
      let extra = invariant n k k in
      inv (n extra)
   (* Uses the following equation to implement pi: 
      pi = Sum (as i goes to infinity) of
      (1/16^i)*( (4/(8i+1))- (2/(8i+4)) - (1/(8i+5)) - (1/(8i+6)) ) *)
  let pi: number = fun n -> 
    let open R in
    if P.(<) n 0 then (one+one+one)
    else
      begin
	let two = number_of_int 2 in
	let four = number_of_int 4 in
	let five = number_of_int 5 in
	let six = number_of_int 6 in
	let eight = number_of_int 8 in
	let sixte = number_of_int 16 in
	let rec exp (num:number) (ex:number) (acc:number) = 
	  if ex === zero then acc
	  else exp num (ex- one) (acc*num) in
	let rec helper (acc:R.number) (k:R.number) = 
	  if P.(=) ((float_of_number k) -. 1.) (float_of_int n) then acc
	  else 
	    let coef = (exp sixte k one) in
	    let frst = four / (eight * k + one) in
	    let secn = two / (eight * k + four) in
	    let thrd = one/(eight * k + five) in
	    let frth = one/(eight * k + six) in
	    let next_add = (inv coef) * (frst - secn - thrd-frth) in
	    helper (acc+ next_add) (k+one) in
	helper zero zero
      end
  (* Uses the equation e= sum n (as n goes to infinity) 1/(n!) *)
  let e:number = fun num ->
    if P.(<) num 0 then number_of_int 3
    else 
      begin 
	let open R in
	let rec factorial (n:number) (acc:number) =
	  if n === zero then acc
	  else factorial (n - one) (acc * n) in
	let rec helper (acc:number) (count:number) (n:int) =
	  if (float_of_int n) = float_of_number count
	  then acc
	  else 
	    let next = inv(factorial count one) in
	    helper (acc+ next) (count+ one) n in
	helper zero zero (P.(+) num 11)
      end
  let format : Format.formatter -> number -> unit = 
    fun f num ->
      R.format f (num (int_of_float (10. ** 64.)))
end
(******************************************************************************)

(*
** vim: ts=2 sw=2 ai et
*)
