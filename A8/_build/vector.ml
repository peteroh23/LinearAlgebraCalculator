(* AF: The float list [[a; b; c; d]] represents the vector [(a; b; c; d)]. [[]]
   is the empty vector. 
 * RI:  *)
type t = float list

let empty = []

let is_empty u = u = []

let cons_v el u = el::u

let make_v lst = lst

let zero_vector n = 
  let rec zeros n acc = 
    if (n = 0) then acc else
      zeros (n - 1) (0.0::acc)
  in zeros n []

let dim_v = List.length

let add_v = List.map2 (+.)

let scale_v c = List.map (fun el -> c *. el)

let elem_v i u = List.nth u (i - 1)

let sub_v u w = add_v u (scale_v (-1.) w )

let dot_v u w = List.fold_left2 (fun acc a b -> acc +. (a *. b)) 0. u w

let mag_v u = sqrt (dot_v u u)

let proj_v u w = scale_v ((dot_v u w)/.(dot_v u u)) u

let hdtl u = match u with
    [] -> failwith "Empty"
  | h::t -> (h,t)

let map f u = List.map f u

let rev = List.rev

let rec has_lead_one u = match u with
    [] -> false
  | h::t -> if h > 0.99999 && h < 1.00001 then true else if h = 0. then has_lead_one t else false

let to_lst_v u = u