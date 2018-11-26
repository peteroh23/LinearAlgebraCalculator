type difficulty = Easy | Hard

exception Empty
exception Malformed

(** [diff_parse s] parses [s] into a difficulty depending on if the user inputs
    easy or hard. Raises: [Empty] if the [s] is empty and [Malformed] if the 
    input is incorrectly formatted. *)
val diff_parse : string -> difficulty

(** [generate_matrix m n] generates a random matrix of dimension [m x n]. Its 
    elements range from 0 to 9. *)
val generate_matrix : int -> int -> Matrix.t

(** [easy_generate m n] is the same as [generate_matrix] except its elements 
    range from 0 to 2. *)
val easy_generate : int -> int -> Matrix.t

(** [check_equal a b] checks if two matrices are equivalent within a small 
    tolerance due to the nature of floating point computation. *)
val check_equal : Matrix.t -> Matrix.t -> bool

(** [mat_format a b] formats two matrices side by side in the terminal. *)
val mat_format : Matrix.t -> Matrix.t -> unit