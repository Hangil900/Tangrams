
module Make(F : Numbers.OrderedField) = struct

  open F
  module U = Numbers.OrderedFieldUtils(F)
  open U
  module R = Region.Make(F)
  type point   = number * number
  type polygon = point list
  type region  = Region.Make(F).region

  let minkowski_difference_convex (obstacle:polygon) (poly:polygon):polygon =
    let minkD obst polyg=  
      let minus p1 p2 = match (p1,p2) with
	| ((x1,y1),(x2,y2)) -> (x1-x2, y1-y2) in
      let rec help2 acc ob poi = match ob with
	| [] -> acc
	| h::t -> help2 ((minus h poi)::acc) t poi in
      let rec helper acc ob pol = match pol with
	|[] -> acc
	| h::t -> helper (help2 acc ob h) ob t in
      helper [] obst polyg in
    (* Calculates the next point in the convex hull *)
    let next_hull points p =
      let turn p1 p2 p3 = match (p1,p2,p3) with
	| ((x1,y1),(x2,y2),(x3,y3)) -> 
	  let cross =  ((x2-x1) * (y3-y1) - (x3-x1) * (y2-y1)) in
	  if cross === zero then 0
	  else if cross < zero then -1
	  else 1 in
      let dist p1 p2 = match (p1,p2) with
	|((x1,y1),(x2,y2)) -> 
	  let dx= x2-x1 in
	  let dy = y2-y1 in
	  dx * dx + dy * dy in
      let rec helper points p q = match points with
	|h::t -> 
	  let turn = turn p q h in
	  if (turn = -1) || ((turn = 0)&& ((dist p h) > (dist p q)))
	  then helper t p h
	  else helper t p q
	| [] -> q in
      helper points p p in
    let find_start points = 
      let smaller p1 p2 = match (p1,p2) with
	| ((x1,y1), (x2,y2)) -> if x1 < x2 then true
	  else if x1 > x2 then false
	  else (y1 < y2) in
      let rec find_min pts p = match pts with
	| h::t -> if smaller p h then find_min t p
	  else find_min t h
	| [] -> p in
      match points with
      | h::t -> find_min t h
      | [] -> failwith "Not a list of points" in
    let rec convex points acc prev start= 
      let nextH = next_hull points prev in
      match (nextH,start) with
      | ((x,y),(x2,y2)) ->
	(* Stops if the original first point has been reached *)
	if (x === x2) && (y === y2) then acc
	else (let newAcc = nextH::acc in
	      convex points newAcc nextH start) in
    let points = minkD obstacle poly in 
    let start = find_start points in
    List.rev(convex points [start] start start)

  let minkowski_difference (obstacles:polygon list) (poly:polygon):region= 
    let rec helper acc obst = match obst with
      | [] -> acc
      | h::t -> 
	begin
	  let conv = minkowski_difference_convex h poly in
	  let reg2 = R.create conv in
	  reg2
	(*
	  let union = R.union acc reg2 in
	  helper union t  
	  end in
	*)
	end in
    helper (R.create poly) obstacles
end

