open Complex

(** [parse_dims dims] parses [dims] to return a tuple representing
    the two numbers in the string [dims].
    Raises: Invalid_argument if there are no valid number representations
    in [dims] *)
let parse_dims (dims:string): int*int = 
  let strlist = String.split_on_char (' ') (String.trim dims) in 
  match List.length(
      strlist) != 2 with
  | true -> raise (Invalid_argument dims)
  | false -> match int_of_string (List.nth strlist 0) with
    | exception Invalid_argument dims  -> raise (Invalid_argument dims)
    |row -> match int_of_string(List.nth strlist 1) with
      | exception Invalid_argument dims  -> raise (Invalid_argument dims)
      |col -> (row,col)

(** [parse_row elts cols] parses [elts] to return a vector type
    that represents a vector with the numbers in [elts].
    Raises: Invalid_argument if the number of numbers in [elts] is
    not equal to [cols]*)
let parse_row (elts:string) (cols:int): Vector.t =
  let strlist = String.split_on_char (' ') (String.trim elts) in 
  match List.length(
      strlist) != cols with
  | true -> raise (Invalid_argument elts)
  | false -> (List.map (fun x -> float_of_string x) strlist) |> Vector.make_v

(** [read_rows rows incs col acc] accumulates the rows of the matrix
    from the user given [rows] and [col] and returns a matrix type with
    these vectors. *)
let rec read_rows (rows:int) (inc:int) (col:int) (acc:Matrix.t) = 
  if inc > rows then acc
  else
    match read_line (print_endline ("\nEnter the elements of row " ^
                                    string_of_int inc ^"\n");
                     print_string "> ") with
    | exception End_of_file -> acc
    | row -> (match read_rows (rows)(inc+1)(col)
                      (Matrix.add_row (parse_row (row)(col)) acc) with 
             exception Failure str1 -> raise (Invalid_argument "str1")
             | m1 -> m1)

type sgn = Pos | Neg | Zero
let sign n = if n = 0. then Zero else if n > 0. then Pos else Neg  

let complex_to_str z = match sign z.im with
    Pos -> string_of_float z.re ^ " + " ^ string_of_float z.im ^ "i"
  | Neg -> string_of_float z.re ^ " - " ^ string_of_float (abs_float z.im) ^ "i"
  | Zero -> string_of_float z.re

let eigvec_to_str lst = List.map (fun el -> complex_to_str el) lst

let rec eigvec_to_out = function
    [] -> ()
  | a::[] -> print_string a
  | h::t -> print_string h; print_string "; "; eigvec_to_out t

let format_eigvec u = print_string "["; u |> eigvec_to_str |> eigvec_to_out;
  print_string "]"

(** [get_identifier] prompts the user for an identifer. *)
let get_identifier a : string = 
  print_endline "\nEnter the identifier of the matrix.\n"; print_string "> ";
  match read_line() with
  | exception End_of_file -> "\nInvalid identifier\n"
  | str -> str

let binary_op (f:Matrix.t -> Matrix.t -> 'a) (dict:(string * Matrix.t) list) = 
  print_endline "\nEnter the dimensions of the matrices.\n"; print_string "> ";
  let str = read_line () in 
  let (m,n) = parse_dims str in
  let a = 
    print_endline "\nDo you want to use a saved matrix for the first matrix?\n
    'Yes' or 'No'\n"; print_string "> ";
    match read_line() with
    | str -> if str = "Yes" || str = "yes"
      then (match (Matrix.get_val (get_identifier "") dict) with
          | Some v -> v
          | None -> Matrix.empty) 
      else (match print_newline (); print_endline
                ("Enter the elements of the first matrix row by row.\n"); 
              print_endline ("Please use spaces to seperate the elements.");
            with
            | () -> Matrix.rev (read_rows m 1 n Matrix.empty)) in           
  let b = 
    print_endline "\nDo you want to use a saved matrix for the second matrix?\n
    'Yes' or 'No'\n"; print_string "> ";
    match read_line() with
    | str -> if str = "Yes" || str = "yes"
      then (match (Matrix.get_val (get_identifier "") dict) with
          | Some v -> v
          | None -> Matrix.empty) 
      else (match print_newline (); print_endline 
                ("Enter the elements of the first matrix row by row.\n");
              print_endline ("Please use spaces to seperate the elements."); 
            with
            | () -> Matrix.rev (read_rows m 1 n Matrix.empty)) in     
  f a b

let unary_op f (dict:(string * Matrix.t) list) = 
  print_endline "\nEnter the dimensions of the matrix.\n"; print_string "> ";
  let str = read_line () in 
  let (m,n) = parse_dims str in
  let a = 
    print_endline "\nDo you want to use a saved matrix?\n
    'Yes' or 'No'\n"; print_string "> ";
    match read_line() with
    | str -> if str = "Yes" || str = "yes" 
      then (match (Matrix.get_val (get_identifier "") dict) with
          | Some v -> v
          | None -> Matrix.empty) 
      else (match print_newline (); 
              print_endline
                ("Enter the elements of the first matrix row by row.\n");
              print_endline ("Please use spaces to seperate the elements.");
            with
            | () -> Matrix.rev (read_rows m 1 n Matrix.empty)) in
  f a


let binary_op_diff f (dict:(string * Matrix.t) list) = 
  print_endline "\nEnter the dimensions of the first matrix.\n";
  print_string "> ";
  let str = read_line () in 
  let (m,n) = parse_dims str in
  let a = 
    print_endline "\nDo you want to use a saved matrix for the first matrix?\n
    'Yes' or 'No'\n"; print_string "> ";
    match read_line() with
    | str -> if str = "Yes" || str = "yes"
      then (match (Matrix.get_val (get_identifier "") dict) with
          | Some v -> v
          | None -> Matrix.empty) 
      else (match print_newline (); 
              print_endline
                ("Enter the elements of the first matrix row by row.\n");
              print_endline ("Please use spaces to seperate the elements.");
            with
            | () -> Matrix.rev (read_rows m 1 n Matrix.empty)) in 

  print_endline "Enter the dimensions of the second matrix.\n"; 
  let str2 = read_line () in 
  let (m2,n2) = parse_dims str2 in
  let b = 
    print_endline "\nDo you want to use a saved matrix for the second matrix?\n
    'Yes' or 'No'\n"; print_string "> ";
    match read_line() with
    | str -> if str = "Yes" || str = "yes" 
      then (match (Matrix.get_val (get_identifier "") dict) with
          | Some v -> v
          | None -> Matrix.empty) 
      else (match print_newline ();
              print_endline ("Enter the elements of the first matrix
               row by row.\n");
              print_endline ("Please use spaces to seperate the elements.");
            with
            | () -> Matrix.rev (read_rows m2 1 n2 Matrix.empty)) in  
  f a b

let to_string (mat:Matrix.t) = print_string "  \n ";
  List.iter (fun lst -> (if lst == List.nth (Matrix.to_lst mat) (0) then () 
                         else print_string(" ");
                         print_string "|"; List.iter (fun el -> print_float el; 
                                                       print_string " ") lst );
              print_string "|";
              if lst == List.nth (Matrix.to_lst mat) ((Matrix.dim_row mat)-1)
              then () else ();
              print_newline ()) (Matrix.to_lst mat)



let commands = ["add"; "subtract"; "multiply"; "scale"; "transpose"; 
                "inverse"; "rref"; "rank"; "#solutions"; "solve"; "trace"; 
                "determinant"; "eigenvalues"; "eigenvectors";"rowspace";
                "columnspace"; "assign"; "get"; 
                "save";"help"; "quiz"; "quit"]

let getc n = List.nth commands n

let format_commands () = print_newline ();
  print_endline ((getc 0) ^ "           " ^ (getc 1) ^ "      " ^ 
                 (getc 2) ^ "      " ^ (getc 3) ^ "          " ^ (getc 4));
  print_endline ((getc 5) ^ "       " ^ (getc 6) ^ "          " ^ 
                 (getc 7) ^ "          " ^ (getc 8) ^ "     " ^ (getc 9));
  print_endline ((getc 10) ^ "         " ^ (getc 11) ^ "   " ^ 
                 (getc 12) ^ "   " ^ (getc 13) ^ "   " ^ (getc 14));
  print_endline ((getc 15) ^ "   " ^ (getc 16) ^ "        " ^ 
                 (getc 17) ^ "           " ^ (getc 18) ^ "           " ^ 
                 (getc 19));
  print_endline ((getc 20) ^ "          " ^ (getc 21)); print_newline ()

let addstr = "'add' adds together two matrices of equal dimension element-wise."
let substr = "'sub' subtracts two matrices of equal dimension element-wise."
let multstr = "'multiply' multiplies two matrices together. If the premultiplier
 has dimension mxn and the postmultiplier has dimension nxl, the resulting 
 matrix will have dimension mxl."
let scstr = "'scale' is the scalar multiplication of a number and a matrix. 
The multiplication is done element-wise with the scalar."
let transstr = "'transpose' is the tranpose of a matrix. The rows and columns
 of the matrix are interchanged." 
let invstr = "'inverse' is the inverse of a matrix. The inverse of a matrix
 multiplied by itself is the identity matrix. Input matrix must be square and
  nonsingular for a valid inverse."
let rrefstr = "'rref' computes the reduced row echelon form of a matrix."
let rankstr = "'rank' is the rank of a matrix. "
let numsolstr = "'#solutions' is the number of solutions that an augmented 
matrix representing a system of linear equations has."
let solvestr = "'solve' is the solution to the linear equations represented by 
the input augmented matrix. Only valid if the number of solutions is unique."
let tracestr = "'trace' is the trace of a matrix. It is the sum of the elements
 along the main diagonal. The matrix must be square."
let detstr = "'determinant' is the determinant of a matrix. The matrix must be 
square and either 2x2, 3x3, or traingular."
let eigvalstr = "'eigenvalues' computes the eigenvalues of a 2x2 matrix. They
 can be real or complex."
let eigvecstr = "'eigenvectors' computes the eigenvectors of a 2x2 matrix. They
 can be real or complex."
let rowstr = "'rowspace' returns the vectors that make up the row space of a
 matrix."
let colstr = "'colspace' returns the vectors that make up the column space of
 a matrix."
let assignstr = "'assign' assigns a name to a given matrix to be used later. 
Similar to assignment '=' in matlab."
let getstr = "'get' returns the matrix that is assigned to a given name."
let savestr = "'save' is used after another operation to save the resulting 
matrix and give it a name to be used later."
let helpstr = "Helpception"
let quizstr = "'quiz' allows you to take a mini quiz to test your knowledge!"
let quitstr = "'quit' is used to exit from the linear algebra calculator.
 Goodbye!"

let descrips = [addstr; substr; multstr; scstr; transstr; invstr; rrefstr;
                rankstr; numsolstr; solvestr; tracestr; detstr; eigvalstr;
                eigvecstr; rowstr;
                colstr; assignstr; getstr; savestr; helpstr; quizstr; quitstr]
let descriptions = List.combine commands descrips


let add_easy () = print_endline "Add together the following matrices:";
  print_newline ();
  let m1 = (Quiz.generate_matrix 2 2) in 
  let m2 = (Quiz.generate_matrix 2 2) in
  Quiz.mat_format m1 m2; print_newline (); 
  print_endline "Input your answer below."; print_newline ();
  let a = (match print_newline (); print_endline ("Enter the elements of the
   answer row by row.\n"); print_endline ("Please use spaces to seperate the 
   elements.\n");  with
          | () -> Matrix.rev (read_rows 2 1 2 Matrix.empty)) in 
  if Quiz.check_equal a (Matrix.add m1 m2) then 
    (print_endline "You're right!"; print_newline (); 1) else 
    (print_endline "Not quite! Good try!"; print_newline (); 0)

let scale_easy () = print_endline "Multiply the following matrix by
 the scalar 3"; print_newline (); 
  let m1 = Quiz.generate_matrix 3 2 in to_string m1; print_newline ();
  print_endline "Input your answer below."; print_newline ();
  let a = (match print_newline (); print_endline ("Enter the elements of the 
  answer row by row.\n"); print_endline ("Please use spaces to seperate the
   elements.\n"); with
          | () -> Matrix.rev (read_rows 3 1 2 Matrix.empty)) in if 
    Quiz.check_equal a (Matrix.scale 3. m1) then 
    (print_endline "You're right!"; print_newline (); 1) 
  else (print_endline "Not quite! Good try!"; print_newline (); 0)

let rref_easy () = print_endline "Put the following matrix into reduced
 row echelon form."; print_newline ();
  let m1 = Quiz.generate_matrix 4 3 in to_string m1; print_newline (); 
  print_endline "Input your answer below."; print_newline ();
  let a = (match print_newline (); print_endline ("Enter 
  the elements of the answer row by row.\n"); print_endline 
               ("Please use spaces to seperate the elements.\n");  with
          | () -> Matrix.rev (read_rows 4 1 3 Matrix.empty)) in if
    Quiz.check_equal a (Matrix.rref_t m1) then 
    (print_endline "You're right!"; print_newline (); 1) else 
    (print_endline "Not quite! Good try!"; print_newline (); 0)

let solve_hard () = print_endline "Solve the following system of linear
 equations represented by the augmented matrix below."; print_newline ();
  let m1 = Quiz.easy_generate 3 4 in to_string m1; print_newline (); 
  print_endline "Input your answer below."; print_newline ();
  let a = (match print_newline (); print_endline ("Please use spaces to
   seperate the elements.\n");  with
          | () -> Matrix.rev (read_rows 1 1 3 Matrix.empty)) in if
    Quiz.check_equal (Matrix.trans a) (Matrix.solve m1) then
    (print_endline "You're right!"; print_newline (); 1) else
    (print_endline "Not quite! Good try!"; print_newline (); 0)

let trace_hard () = print_endline "What is the trace of the following matrix?";
  print_newline ();
  let m1 = Quiz.generate_matrix 5 5 in to_string m1; print_newline ();
  print_endline "Input your answer below."; print_newline ();
  let a = (match print_newline (); print_string "> " with
      | () -> Matrix.rev (read_rows 1 1 1 Matrix.empty)) in 
  if Quiz.check_equal a (Matrix.add_row (Vector.cons_v (Matrix.trace m1) 
                                           Vector.empty) Matrix.empty) then
    (print_endline "You're right!";
     print_newline (); 1) else (print_endline "Not quite! Good try!"; 
                                print_newline (); 0)

let multiply_hard () = print_endline "Multiply the following two matrices.";
  print_newline ();
  let m1 = (Quiz.generate_matrix 5 5) in 
  let m2 = (Quiz.generate_matrix 5 5) in
  Quiz.mat_format m1 m2; print_newline (); print_endline 
    "Input your answer below: ";
  let a = (match print_newline ();  with
      | () -> Matrix.rev (read_rows 5 1 5 Matrix.empty)) in if
    Quiz.check_equal a (Matrix.mult m1 m2)  then 
    (print_endline "You're right!"; print_newline (); 1) else 
    (print_endline "Not quite! Good try!"; print_newline (); 0)


(** [run] runs the interface for the user until exited. *)
let rec run (dict:(string * Matrix.t) list) (prevmat:Matrix.t) = 
  let safe_quiz q = match q () with 
    exception Invalid_argument s -> print_endline
                                      "\nImproper input. Moving on."; 0
    | num -> num in
  let format f g d = print_newline (); match f g d with
    exception End_of_file -> 
      print_endline "\nPlease try again."; run dict prevmat
    | exception Invalid_argument d -> 
      print_endline "\nImproper input. Try again."; run dict prevmat
    | t -> to_string t; run dict t in 
  print_newline (); print_endline "Enter a command.\n"; print_string "> ";
  (match read_line () with
   exception End_of_file -> run dict prevmat
   | s -> match Operation.parse s with
     exception Operation.Empty -> run dict prevmat
     | exception Operation.Malformed -> 
       print_endline "\nCommand not input properly."; run dict prevmat
     | Quiz -> print_newline (); print_endline 
         "\nYou have decided to take the quiz!"; print_endline 
         "\nDo you want it to be 'easy' or 'hard'?\n"; 
       print_string "> "; (match read_line () with
         exception End_of_file -> run dict prevmat
         | input -> (match Quiz.diff_parse input with
             exception Quiz.Empty -> print_endline "You didn't input anything.";
             run dict prevmat
             | exception Quiz.Malformed -> print_endline 
                                             "Command not input properly.";
               run dict prevmat
             | Easy -> print_newline (); let number = safe_quiz (rref_easy) +
                                                      safe_quiz (scale_easy) +
                                                      safe_quiz (add_easy) in 
               print_newline (); print_endline ("\nYou got " ^ 
                                                (string_of_int number) ^ 
                                                "/3 correct. Great work!");
               run dict prevmat
             | Hard -> print_newline (); let number = safe_quiz multiply_hard +
                                                      safe_quiz solve_hard + 
                                                      safe_quiz trace_hard in
               print_newline ();
               print_endline ("You got " ^ (string_of_int number) ^ "/3 correct.
                Great work!"); run dict prevmat))
     | Add -> format (binary_op) (Matrix.add) (dict)
     | Subtract -> format (binary_op) (Matrix.sub) (dict)
     | Multiply -> (match format binary_op_diff Matrix.mult dict with
         exception Matrix.DimUnequal -> 
         print_endline "\n The dimensions of the matrices 
         were not compatible for multiplication. Try again."; run dict prevmat
         | t -> run dict prevmat)
     | Scale -> print_endline "\n Input a scalar you would like to 
     multiply by.\n"; print_string "> ";
       (match read_line () with
        exception End_of_file -> 
         print_endline "\nNothing was entered. Try again!"; run dict prevmat
        | str -> let c = float_of_string str in 
          format unary_op (Matrix.scale c) (dict))
     | Transpose -> format unary_op Matrix.trans dict
     | Inverse -> (match format unary_op Matrix.inverse dict with
         exception Matrix.NotSquare -> 
         print_endline "\nThis matrix is not square. Try again."; 
         run dict prevmat
         | exception Matrix.NotInvertible -> 
           print_endline "\nThis matrix cannot be inverted. Try again.";
           run dict prevmat
         | t -> run dict prevmat)
     | Rref -> format unary_op Matrix.rref_t dict
     | Solve -> format unary_op Matrix.solve dict
     | Rank -> print_newline (); (match unary_op Matrix.rank dict with 
         exception End_of_file -> print_endline "\nPlease try again."; 
         run dict prevmat
         | exception Invalid_argument d -> print_endline "\nImproper input. 
         Try again."; run dict prevmat
         | t -> print_newline (); print_string "\nThe rank of this matrix is: "; 
           print_int t; run dict prevmat)
     | NumSols -> print_newline (); (match unary_op Matrix.num_sols dict with
         exception End_of_file -> print_endline "\nPlease try again."; 
         run dict prevmat
         | exception Invalid_argument d -> print_endline "\nImproper input. 
         Try again."; run dict prevmat
         | `NoSol -> print_endline "\nThis matrix has no solution."; 
           run dict prevmat
         | `UniqueSol -> print_endline "\nThis matrix has a unique solution.";
           run dict prevmat
         | `InfiniteSol -> print_endline "\nThis matrix has an infinite number 
         of solutions."; run dict prevmat)
     | Trace -> print_newline (); print_string "\nPlease enter a square
      matrix."; print_newline (); (match unary_op Matrix.trace dict with
         exception End_of_file -> print_endline "\nPlease try again."; 
         run dict prevmat
         | exception Invalid_argument d -> print_endline "\nImproper input.
          Try again."; run dict prevmat
         |  exception Matrix.NotSquare -> 
           print_endline "\nThis matrix is not square. Try again.";
           run dict prevmat
         | t -> print_newline (); 
           print_string "\nThe trace of this matrix is: "; 
           print_float t; run dict prevmat)
     | Determinant -> print_newline (); print_string "\nPlease enter 2x2, 3x3,
      or triangular matrix."; print_newline (); 
       (match unary_op Matrix.determinant_main dict with
        exception End_of_file -> print_endline "\nPlease try again.";
         run dict prevmat
        | exception Invalid_argument d -> print_endline "\nImproper input.
         Try again."; run dict prevmat
        | exception Matrix.NotTwoByTwo -> print_newline (); 
          print_string "\nInvalid argument1. Try again."; run dict prevmat
        | exception Matrix.NotThreeByThree -> print_newline (); 
          print_string "\nInvalid argument2. Try again."; run dict prevmat
        | exception Matrix.NotTriangular -> print_newline (); 
          print_string "\nInvalid argument3. Try again."; run dict prevmat
        | exception Matrix.NotSquare -> print_newline (); 
          print_string "\nThis matrix is not square. Try again."; 
          run dict prevmat
        | t -> print_newline (); print_string "\nThe determinant
         of this matrix is: "; 
          print_float t; run dict prevmat)
     | Eigvals -> print_newline (); print_string "\nPlease enter a 2x2 matrix.";
       print_newline ();
       (match unary_op Matrix.eigval2x2 dict with 
        exception End_of_file -> print_endline "\nPlease try again."; 
         run dict prevmat
        | exception Invalid_argument d -> print_endline "Improper input. 
        Try again."; run dict prevmat
        | exception Matrix.NotTwoByTwo -> print_newline (); print_string 
            "Invalid argument1. Try again."; run dict prevmat
        | (eig1,eig2) -> print_newline (); print_string ("The eigenvalues are: 
        " ^ complex_to_str eig1 ^ " and " ^ complex_to_str eig2)); 
       print_newline (); run dict prevmat
     | Eigvects -> print_newline (); print_string "Please enter a 2x2 matrix.";
       print_newline ();
       (match unary_op Matrix.eigvec2x2 dict with 
        exception End_of_file -> print_endline "Please try again."; 
         run dict prevmat
        | exception Invalid_argument d -> print_endline "\nImproper input. 
        Try again."; run dict prevmat
        | exception Matrix.NotTwoByTwo -> print_newline (); print_string 
            "\nInvalid argument1. Try again."; run dict prevmat
        | (vec1,vec2) -> print_newline (); print_string "\nThe eigenvectors 
        are: "; format_eigvec vec1; print_string " and "; format_eigvec vec2); 
       print_newline (); run dict prevmat
     | Rowspace -> print_newline (); format unary_op Matrix.row_space dict
     | Colspace -> print_newline (); format unary_op Matrix.col_space dict
     | Assign -> let iden = get_identifier "" in 
       let new_dict = Matrix.insert_dict iden (unary_op (Matrix.no_op) dict) 
           (dict) in 
       run new_dict prevmat
     | Get -> (match Matrix.get_val (get_identifier "") (dict) with 
         | Some v ->  
           print_string " ";
           to_string v; run dict prevmat
         | None -> print_endline "\nThe identifier you entered does not exist.";
           run dict prevmat)
     | Save -> let iden = get_identifier "" in 
       let new_dict = Matrix.insert_dict iden prevmat dict in 
       run new_dict prevmat
     | Help str -> (match List.assoc_opt str descriptions with
           Some descrip ->  print_newline (); print_endline descrip; 
           run dict prevmat
         | None -> print_newline (); print_endline  "\nThe following are 
         valid commands: ";
           format_commands (); print_endline "\nType in the command followed by 
           a '?' for more help."; run dict prevmat)
     | Quit -> print_newline (); print_endline "\nSee you next time!"; exit 0)


(** [main] serves as the main method for the program. *)
let main () = 
  print_endline("\n\nWelcome to the Linear Algebra Calculator!!!\n");
  print_newline (); print_endline "With this calculator, you can perform
  numerous computations involving matrices.";
  print_newline (); print_endline "You can also take a randomly generated
  quiz to test out your linear algebra skills!! \n";
  print_newline (); print_endline "There is an easy and hard version of the 
  quiz.";
  print_newline (); print_endline ("Enter one of the following commands: ");
  format_commands ();
  print_endline "If you need help with a specific command, type in the command 
  followed by a '?'.";
  let dict = Matrix.empty_dict in 
  run dict Matrix.empty

(* Execute the game engine. *)
let () = main ()

