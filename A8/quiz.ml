type difficulty = Easy | Hard

exception Empty
exception Malformed

let to_list_str str = 
  List.filter (fun el -> el <> "") (String.split_on_char ' ' str)

let diff_parse str = match to_list_str str with
    [] -> raise Empty
  | h::[] -> (match h with
        "easy" -> Easy
      | "ez" -> Easy
      | "Easy" -> Easy
      | "EZ" -> Easy
      | "e" -> Easy
      | "E" -> Easy
      | "hard" -> Hard
      | "Hard" -> Hard
      | "expert" -> Hard
      | "h" -> Hard
      | "H" -> Hard
      | _ -> raise Malformed)
  | _ -> raise Malformed

let generate_vector n = Random.self_init (); 
  let rec generv n acc = if n = 0 then acc 
    else generv (n-1) (Vector.cons_v (float_of_int (Random.int 10)) acc) 
  in generv n Vector.empty

let generate_matrix m n = 
  let rec generm m n acc = if m = 0 then acc else generm (m-1) n 
        (Matrix.add_row (generate_vector n) acc) 
  in generm m n Matrix.empty

let easy_generate m n = 
  let rec generv n acc = if n = 0 then acc 
    else generv (n-1) (Vector.cons_v (float_of_int (Random.int 3)) acc) in
  let rec generm m n acc = if m = 0 then acc else generm (m-1) n 
        (Matrix.add_row (generv n Vector.empty) acc) 
  in generm m n Matrix.empty

let tol x y = (abs_float (x -. y)) < 0.00001

let check_equal a b = 
  let a_lst = List.flatten (Matrix.to_lst a) in 
  let b_lst = List.flatten (Matrix.to_lst b) in List.for_all2 tol a_lst b_lst

let mat_format m1 m2 = print_string " ";
  List.iter2 (fun v1 v2 -> (if v1 == List.nth (Matrix.to_lst m1) (0) then () 
                            else print_string(" ");
                            print_string "|"; List.iter (fun el -> 
                                print_float el; print_string " ") v1; 
                            print_string "|"; print_string "     ";
                            print_string "|"; List.iter (fun el -> 
                                print_float el; print_string " ") v2; 
                            print_string "|"); print_newline ()) 
    (Matrix.to_lst m1) (Matrix.to_lst m2)


