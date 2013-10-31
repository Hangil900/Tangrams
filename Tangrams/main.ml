
(* Change NT to whatever number type you want to use. *)


module NT = Numbers.Floats
module UI = Ui.Make(NT)

let () = UI.main ()
