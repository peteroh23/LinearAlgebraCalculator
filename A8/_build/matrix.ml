
(* AF: The vector list [[v1; v2; v3; v4]] represents the matrix with row vectors 
   v1, v2, v3, and v4 ordered from top to bottom. [[]] is the empty matrix.
 * RI: All of the row vectors have equal dimension. *)
open Vector 
open Complex

type t = Vector.t list

exception DimUnequal
exception Exit
exception NotInvertible
exception NotSquare
exception NotTwoByTwo
exception NotThreeByThree
exception NotTriangular

let empty : t = []

let is_empty a = a = []

let empty_dict : (string * t) list = []

let insert_dict (k:string) (v:t) (d:(string * t) list) : (string * t) list =
  let rec inserthelp (dict:(string * t) list) (acc:(string * t) list) : 
    (string * t) list = 
    match dict with 
    |[] -> acc@[k,v]
    |h::t -> (match h with |(hk,hv) -> if k = hk then inserthelp t acc 
        else inserthelp t acc@[h]) in 
  inserthelp d []

let rec get_val (k:string) (d:(string * t) list) : t option = 
  match d with 
  |[] -> None
  |(k',v') :: t -> if k=k' then Some v' else get_val k t

let no_op (mat:t) : t = mat

let zero_matrix m n = 
  let rec zeros m n acc = 
    if (m = 0) then acc else
      zeros (m-1) (n) ((Vector.zero_vector n)::acc)
  in zeros m n []

let dim_hd a = match a with
    [] -> 0
  | h::t -> Vector.dim_v h

let dim_row a = List.length a

(* [is_valid a] checks whether [a] is a valid matrix. This means all of its row
    vectors have the same dimension. *)
let rec is_valid a = let dim_hd' = dim_hd a in 
  List.for_all (fun u -> dim_hd' = Vector.dim_v u) a

let rep_ok a = let debug = true in
  if debug then 
    if is_valid a then a else failwith "RI" 
  else a

let dim a = let a' = rep_ok a in (List.length a', dim_hd a')

let is_square a = let (m, n) = dim (rep_ok a) in m = n

let elem i j a = Vector.elem_v j (List.nth a (i - 1))

let rev = List.rev

let add_row u a = if is_empty a then u::a else 
  if Vector.dim_v u = dim_hd a then u::a else raise DimUnequal

let scale c = List.map (Vector.scale_v c)

let add a b : t = if dim a = dim b 
  then List.map2 Vector.add_v a b else raise DimUnequal

let sub a b = if dim a = dim b
  then add a (scale (-1.0) b) else raise DimUnequal

let add_col u a = let rec add_c u a acc = match a with
      [] -> acc
    | h::t ->  match Vector.hdtl u with
      exception Failure s -> raise (Invalid_argument "Vector Empty")
      | hd, tl -> add_c tl t ((Vector.cons_v hd h)::acc)
  in if is_empty a then 
    List.rev (add_c u (Vector.map (fun el -> Vector.empty) u) [])
  else (List.rev (add_c u a empty))

let trans a = List.fold_right (fun el acc -> add_col el acc) a []

let mult a b = let n = dim_hd a in if n = dim_row b then
    List.fold_right 
      (fun u init -> 
         add_row (Vector.make_v (List.map 
                                   (fun w -> 
                                      Vector.dot_v u w) (trans b))) init) a []
  else raise DimUnequal

let to_lst = List.map Vector.to_lst_v

let to_array (m:t) = 
  let dimx = dim_row m in 
  let dimy = dim_hd m in 
  let z = Array.make_matrix dimx dimy 0.0 in
  for x = 0 to (dimx-1) do
    for y = 0 to (dimy-1) do
      z.(x).(y) <- (elem (x+1) (y+1) m)
    done;
  done;
  z

(** [row_array r1 m] takes a float array array [m] and returns the row vector
    at [r1]. *)
let row_array r1 m : float array =
  Array.get m (r1)

(** [col_array c1 m] takes a float array array [m] and returns the 
    column vector at [c1]. *)
let col_array c1 m : float array = 
  let row = Array.length m in 
  let newArray = ref (Array.make 0 0.0) in
  for x = 0 to (row-1) do
    let temp = m.(x).(c1) in
    newArray := Array.append (!newArray)(Array.make 1 temp);
  done;
  !newArray

(** [add_row2 r1 m v] takes a float array array [m] and adds the float array [v]
    at row [r1]. *)
let add_row2 r1 m v : float array array = 
  m.(r1) <- v;
  m

(** [exchange_rows r1 r2 m] takes a float array array [m] and interchanges
    the rows of [r1] and [r2]. *)
let exchange_rows r1 r2 m =
  let row1 = row_array r1 m in 
  let row2 = row_array r2 m in 
  m.(r1) <- row2;
  m.(r2) <- row1

(** [scale_row r1 s m] takes a float array array [m] and a scalar value [s] 
    and scales the vector at row [r1]. *)
let scale_row r1 s m = 
  for x = 0 to (Array.length m.(0) - 1) do
    m.(r1).(x) <- (m.(r1).(x) *. (1.0/.s))
  done

(** [scale_and_subtract r1 r2 s m] takes a float array array [m] and a scalar
    value [s] and subtracts the product of [s] and [r2] from [r1]. *)
let scale_and_subtract r1 r2 s m =
  for x = 0 to (Array.length m.(0) - 1) do
    let z = ref (m.(r2).(x)) in
    let w = ref (m.(r1).(x)) in
    m.(r1).(x) <- (!w -. (s *. !z))
  done

(** [rref m] takes a float array array and modifies the 
    matrix so that it is in Reduced Row Echelon Form. 
    The pseudocode for this algorithm was taken from
    https://rosettacode.org/wiki/Reduced_row_echelon_form .*)
let rref (m:float array array) = 
  try
    let lead = ref 0 in
    let rowCount = Array.length m in 
    let colCount = Array.length m.(0) in 
    for x = 0 to (rowCount - 1) do
      if (colCount <= !lead) then
        raise Exit;
      let i = ref x in
      while (m.(!i).(!lead) = 0.0) do
        i := !i +1;
        if (rowCount = !i ) then
          i := x;
        lead := !lead +1;
        if (colCount = !lead) then 
          raise Exit;
      done;
      exchange_rows (!i)(x)(m);
      let temp = (m.(x).(!lead)) in
      if not (temp = 0.0) then 
        scale_row x temp m;
      for i = 0 to (rowCount -1) do
        if not (i = x) then
          let temp2 = m.(i).(!lead) in
          scale_and_subtract (i)(x)(temp2)(m);
      done;
      lead := !lead +1;
    done;
  with Exit -> ();;


(** [rref2 m] takes a type t, runs [rref], changes -0.0 to 0.0,
    and returns the modified array. *)
let rref2 m = 
  let m1 = to_array m in
  rref m1;
  let row = Array.length m1 in
  let col = Array.length m1.(0) in 
  for x = 0 to (row -1) do
    for y = 0 to (col -1) do 
      if (m1.(x).(y) = -0.) then
        m1.(x).(y) <- 0.;
    done;
  done;
  m1

(** [aryto_t_help row ary acc] converts [ary], which has [row] rows,
    into a Matrix.t representation . *)
let rec aryto_t_help (row:int)(ary:float array array)(acc:t): t =
  if row = Array.length ary then acc else
    aryto_t_help (row+1) (ary) (add_row ((Array.to_list (ary.(row))) 
                                         |> Vector.make_v)(acc))

let array_to_t m : t = 
  List.rev(aryto_t_help (0) (m) (empty))

let rref_t m = m |> rref2 |> array_to_t

let check_identity m = 
  let row = Array.length m in
  let col = Array.length m.(0) in
  let lead = ref 0 in
  let currentBool = ref true in 
  for x = 0 to (row -1) do
    for y = 0 to (col -1) do
      if (!lead = y) then
        if not (m.(x).(!lead) = 1.0) then
          currentBool := false;
      if not (!lead = y) then
        if not (m.(x).(y) = 0.0) then
          currentBool := false;
    done;
    lead := !lead +1;
  done;
  !currentBool

let inverse m = 
  let dimension = is_square m in 
  if dimension then
    let m1 = to_array m in
    let row = Array.length m1 in 
    let lead = ref 0 in 
    for x = 0 to (row-1) do 
      let tempArray = (Array.make row 0.0) in 
      tempArray.(!lead) <- 1.0; 
      m1.(x) <- Array.append (m1.(x))(tempArray);
      lead := !lead +1;
    done;
    rref m1;
    let row1 = Array.length m1 in 
    let finalArray = (Array.make_matrix row row 0.0) in
    for x = 0 to (row1-1) do
      finalArray.(x) <- (Array.sub (m1.(x)) row row1)
    done;
    let identityArray = (Array.make_matrix row row 0.0) in
    for z = 0 to (row1-1) do
      identityArray.(z) <- (Array.sub (m1.(z)) 0 row)
    done;
    if (check_identity identityArray) then
      array_to_t finalArray
    else
      raise NotInvertible
  else raise NotSquare

let determinant_triangular m = 
  if (is_square m) then
    let m1 = to_array m in 
    let row = Array.length m1 in 
    let lead = ref 0 in
    let acc = ref 1.0 in
    for x = 0 to (row-1) do
      acc := m1.(x).(!lead) *. !acc;
      lead := !lead + 1;
    done;
    !acc;
  else
    raise NotSquare

let determinant_two m = 
  if (is_square m) then
    if (dim m = (2,2)) then
      (elem 1 1 m *. elem 2 2 m) -. (elem 2 1 m *. elem 1 2 m)
    else
      raise NotTwoByTwo
  else 
    raise NotSquare

let determinant_three m =
  if (is_square m) then
    if (dim m = (3,3)) then
      (elem 1 1 m *. ((elem 2 2 m *. elem 3 3 m)-.(elem 3 2 m *. elem 2 3 m)))
      -. 
      (elem 1 2 m *. ((elem 2 1 m *. elem 3 3 m)-.(elem 3 1 m *. elem 2 3 m)))
      +. 
      (elem 1 3 m *. ((elem 2 1 m *. elem 3 2 m)-.(elem 3 1 m *. elem 2 2 m)))
    else
      raise NotThreeByThree
  else 
    raise NotSquare

let is_upper_triangular m = 
  if (is_square m) then
    let m1 = to_array m in 
    let lead = ref 1 in 
    let row = Array.length m1 in 
    let final = ref true in
    for x = 0 to (row-2) do
      for y = !lead to (row-1) do
        if not (m1.(x).(y) = 0.0) then
          final := false;
      done;
      lead := !lead + 1;
    done;
    !final
  else raise NotSquare

let is_lower_triangular m = 
  if (is_square m) then
    let m1 = to_array m in 
    let lead = ref ((Array.length m1) - 2) in 
    let row = Array.length m1 in 
    let final = ref true in 
    for x = (row-1) downto 1 do
      for y = 0 to !lead do
        if not (m1.(x).(y) = 0.0) then
          final := false;
      done;
      lead := !lead - 1;
    done;
    !final
  else raise NotSquare

let determinant_main m = 
  if (is_square m) then
    let result = ref 0.0 in 
    if (dim_hd m = 1) then 
      result := (elem 0 0 m);
    if (dim_hd m = 2) then 
      result := (determinant_two m);
    if (dim_hd m = 3) then
      result := (determinant_three m)
    else
    if (is_lower_triangular m || is_upper_triangular m) then
      result := (determinant_triangular m)
    else
      raise NotTriangular;
    !result;
  else raise NotSquare


let solve m = 
  let get_sol a = 
    List.fold_right (fun el acc -> match Vector.to_lst_v (Vector.rev el) with
          [] -> raise (Invalid_argument "Empty")
        | h::t -> (Vector.make_v [h])::acc) a []
  in m |> rref2 |> array_to_t |> get_sol

let rank m = 
  List.fold_left (fun acc el -> 
      if Vector.has_lead_one el then acc + 1 else acc) 0 (rref_t m)

(* [split_aug m] splits an augmented matrix into a pair of the coefficient 
   matrix
   and solution matrix. *)
let split_aug m = let (u, a) = List.split (List.map (fun el -> 
    let (h, t) = (el |> Vector.rev |> Vector.hdtl) in (h, Vector.rev t)) m)
  in (add_col (Vector.make_v u) empty, a)

let num_sols m = let (_, b) = split_aug m in 
  let (r1, r2) = (rank m, rank b) in 
  if r1 > r2 then `NoSol else 
  if r1 = r2 && r1 = dim_hd b then `UniqueSol else `InfiniteSol 

let trace a = let (m,n) = dim a in if m = n then 
    let rec sum_els b c acc = match b with 
        [] -> acc
      | h::t -> sum_els t (c + 1) ((Vector.elem_v c h) +. acc)
    in sum_els a 1 0.
  else raise NotSquare

let eigval2x2 (a:t) = if dim a = (2,2) then 
    let tr = {re = trace a; im = 0.} in
    let det = determinant_two a in
    let trdiv2 = Complex.div tr {re = 2.; im = 0.} in
    let sqrt_val = 
      Complex.sqrt (Complex.sub ((Complex.div 
                                    (Complex.pow tr {re = 2.; im = 0.}) 
                                    {re = 4.; im = 0.})) {re = det; im = 0.}) in
    (Complex.add trdiv2 sqrt_val, Complex.sub trdiv2 sqrt_val)                      
  else raise NotTwoByTwo

let eigvec2x2 (m:t) = if dim m = (2,2) then
    let b = elem 1 2 m in
    let c = elem 2 1 m in
    if b = 0. && c = 0. then ([Complex.one; Complex.zero], 
                              [Complex.zero; Complex.one]) 
    else 
      let (eig1, eig2) = eigval2x2 m in
      if c <> 0. then 
        let d = elem 2 2 m in
        ([Complex.sub eig1 {re = d; im = 0.}; {re = c; im = 0.}], 
         [Complex.sub eig2 {re = d; im = 0.}; {re = c; im = 0.}])
      else 
        let a = elem 1 1 m in
        ([{re = b; im = 0.}; Complex.sub eig1 {re = a; im = 0.}], 
         [{re = b; im = 0.}; Complex.sub eig2 {re = a; im = 0.}])
  else raise NotTwoByTwo

let row_space m = 
  let m1 = rref2 m in 
  let pivotRows = ref ([]) in 
  let col = Array.length m1.(0) in 
  let lead = ref 0 in 
  for x = 0 to (col-1) do
    if (m1.(!lead).(x) = 1.0) then
      pivotRows := List.cons (!lead)(!pivotRows);
    lead := !lead +1;
  done;
  let rec func lst m m1 =
    match lst with
    | [] -> m1
    | h::t -> 
      func t m (add_row (Vector.make_v (Array.to_list (row_array h m))) m1) in
  func (!pivotRows)(to_array m)(empty)

let col_space m = 
  let m1 = rref2 m in 
  let pivotRows = ref ([]) in 
  let col = Array.length m1.(0) in 
  let lead = ref 0 in 
  for x = 0 to (col-1) do
    if (m1.(!lead).(x) = 1.0) then
      pivotRows := List.cons (x)(!pivotRows);
    lead := !lead +1;
  done;
  let rec func lst m m1 =
    match lst with
    | [] -> m1
    | h::t -> 
      func t m (add_row (Vector.make_v (Array.to_list (col_array h m))) m1) in
  func (!pivotRows)(to_array m)(empty)












