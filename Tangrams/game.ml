
module Make (F : Numbers.OrderedField) = struct
  module Reg = Region.Make(F)
  module Geo = Geometry.Make(F)
  module UTIL = Numbers.OrderedFieldUtils(F)

  type point   = F.number * F.number
  type polygon = point list

  let shapes : polygon list ref = ref []
  let currP: polygon option ref = ref None
  let objVecs: point list ref = ref []
  let minkReg: Reg.region option ref = ref None

  let create_shape p =
    shapes := p :: !shapes
      
  let click (p:point): unit   =
    let append lst lst2 = List.rev_append (List.rev lst) lst2 in
    (* Returns a tuple:  polygon clicked on * the other obstacles*)
    let rec find_curr poi lst acc = match lst with
      | h::t -> if Reg.contains poi (Reg.create h) 
	then (Some h, append acc t)
	else find_curr poi t (append acc [h])
      | [] -> (None, acc) in
    (*Centers the polygon at the origin based on the clicked point*)
    let update_objVecs (poi: point) = match !currP with
      | None -> ()
      | Some pol ->
	let open UTIL in
	let helper acc curr = match (curr, poi) with
	  | ((x,y), (x2, y2)) -> ((x-x2), (y-y2))::acc in
	let obj_Vecs = (List.fold_left helper  [] pol) in
	objVecs := obj_Vecs in
    (*The Minkowski diff based on ObjVecs:Polygon centered at origin*)
    let update_MinkReg () = match !currP with
      (*If no object is clicked on, no need to calc MinkReg*)
      | None -> ()
      | Some _ ->
	let mink_Reg = Geo.minkowski_difference (!shapes) (!objVecs) in
	minkReg := Some mink_Reg in
    let (poly,rest) = find_curr p !shapes [] in
    currP := poly;
    shapes := rest;
    update_objVecs p;
    update_MinkReg ()

  let move_to (p:point):unit  = match !minkReg with
    (* No object is clicked on, so do nothing *)
    | None -> ()
    | Some reg ->
      begin
	(* Adds ObjVecs to the point to represent the object*) 
	let update_currP (p:point) = 
	  let open UTIL in
	  let helper acc curr = match (p, curr) with
	    | ((x,y),(x2,y2)) ->(F.(+) x x2, F.(+) y y2)::acc in
	  currP := Some( List.fold_left helper [] !objVecs) in
	let closest = Reg.find_closest reg p in
	update_currP closest
      end

  let unclick () = match !currP with
    | None -> ()
    | Some pol ->
      (*Adds the finalized polygon position to shapes*)
      create_shape pol; 
      objVecs := [];
      minkReg := None;
      currP := None



  let obstacles ():polygon list = !shapes

  let selection (): polygon option = !currP

  let extra_points () = match !shapes with
    | h::t -> h
    | [] -> []
(*
    let rec points acc lst = match lst with
      | h::t-> points (acc @ h) t
      | [] -> acc in
    points [] !shapes *)

let extra_lines ()  = []

end


