open Vector

(** List based matrix composed of vectors. *)
type t

(** [DimUnequal] is raised whenever an input does not have the correct 
    dimensions *)
exception DimUnequal

(** [Exit] is raised primarily as a helper for the function [rref]. It is raised 
    when the function needs to exit the for-loop.  *)
exception Exit

(** [NotInvertible] is raised whenever an input matrix is not invertible. *)
exception NotInvertible

(** [NotSquare] is raised whenever an input matrix is not a square matrix. *)
exception NotSquare

(** [NotTwoByTwo] is raised whenever an input matrix is by 2 x 2 dimensions. *)
exception NotTwoByTwo

(** [NotThreeByThree] is raised whenever an input matrix is by 3 x 3 dimensions.
*)
exception NotThreeByThree

(** [NotTriangular] is raised whenever an input matrix is not an upper/lower 
    triangular matrix. *)
exception NotTriangular


(** [empty] is the empty matrix, that is a matrix with no row vectors. *)
val empty : t

(** [is_empty a] checks whether [a] is the empty matrix. *)
val is_empty : t -> bool

(** [empty_dict] is a empty dictionary that maps string keys
    to matrix values. *)
val empty_dict : (string * t) list

(** [insert_dict k v d] inserts the pair ([k],[v]) into [d]. *)
val insert_dict : string -> t -> (string * t) list -> (string * t) list

(** [get_val k d] returns Some v if [k] exists in [d]. Else None is
    returned.*)
val get_val : string -> (string * t) list -> t option

(** [no_op mat] returns [mat] without making any changes to it. *)
val no_op : t -> t

(** [zero_matrix m n lst] creates a zero vector with (m x n) dimensions *)
val zero_matrix: int -> int -> t

(** [dim_hd a] is the dimension of the first row vector in [a]. Dimension is 0 
    if matrix is empty. *)
val dim_hd : t -> int

(** [dim_row a] is the number of rows in matrix [a] *)
val dim_row: t -> int

(** [dim a] is the dimension of the matrix [a] in the form of row by column. *)
val dim : t -> int * int

(** [is_valid a] checks whether [a] is a valid matrix. This means all of its row
    vectors have the same dimension. *)
val is_valid : t -> bool

(** [is_square a] checks whether the matrix [a] is square. *)
val is_square : t -> bool

(** [elem i j a] is the element in matrix [a] at the ith row and jth column. 
    Raises: [Failure "nth"] if one of the indices is out of bounds or 
    [Invalid_argument] if one of the indices is <= 0. *)
val elem : int -> int -> t -> float

(** [rev a] is reverses the order of the row vectors in matrix [a]. *)
val rev : t -> t

(** [rep_ok a] checks whether the representation invariant is satisfied for
    argument [a]. If it is, returns matrix [a]. If it fails, raises 
    [Failure "RI"]. *)
val rep_ok : t -> t

(** [add_row u a] adds vector [u] as the first row vector in matrix [a]. Raises:
    [DimUnequal] if the dimension of [u] and number of columns in [a] is 
    unequal. *)
val add_row : Vector.t -> t -> t

(** [add a b] is the addition of two matrices element wise. 
    Raises: [DimUnequal] if the dimension of [a] is not the same as 
    dimension [b] *)
val add : t -> t -> t

(** [sub a b] is the subtraction of two matrices element wise, b subtracted by 
    a. 
    Raises: [DimUnequal] if the dimension of [a] is not the 
    same as dimension [b] *)
val sub : t -> t -> t

(** [scale c a] is the scalar multiplication of matrix [a] by scalar [c]. *)
val scale : float -> t -> t

(** [add_col u a] adds a vector [u] as the first column in matrix [a]. *)
val add_col : Vector.t -> t -> t

(** [trans a] is the transpose of the matrix [a], that is the rows and columns
    are swapped. *)
val trans : t -> t

(** [mult a b] is the matrix multiplication of [a] and [b]. If dimension of [a]
    is [(m, n)], then the dimension of [b] must be [(n, m)], otherwise raises 
    [DimUnequal]. *)
val mult : t -> t -> t

(** [to_lst a] is the list representation of the matrix [a]. *)
val to_lst : t -> float list list

(** [to_array m] takes in a matrix [m] and returns the same matrix
    as an .*)
val to_array: t -> float array array

(** [check_identity m] takes in a matrix [m] and returns true if 
    the matrix is a square, identity matrix. *)
val check_identity: float array array -> bool

(** [array_to_t m] returns the contents of [m] as a Matrix.t representation . *)
val array_to_t: float array array -> t

(** [inverse m] finds the inverse of matrix [m]. Raises: [NotInvertible] if the
    matrix is singular or [NotSquare] if the matrix is not square. *)
val inverse: t -> t

(** [rref_t m] takes in a matrix [m] and returns the reduced-row-echelon
    form of [m]. *)
val rref_t : t -> t

(** [determinant m] is the determinant of a square, triangular matrix [m]. 
    Raises: [NotSquare] if the matrix is not square. *)
val determinant_triangular: t -> float

(** [determinant_two m] is the determinant of a square, 2x2 matrix.
    Raises: [NotSquare] if the matrix is not square.
    Raises: [NotTwoByTwo] if the matrix is not 2 x 2 dimensions. *)
val determinant_two : t -> float

(** [determinant_three m] is the determinant of a square, 3x3 matrix.
    Raises: [NotSquare] if the matrix is not square.
    Raises: [NotThreeByThree] if the matrix is not 3 x 3 dimensions. *)
val determinant_three : t -> float

(** [determinant_main m] is the determinant of a square matrix. 
    Raises: [NotSquare] if the matrix is not square.
    Raises: [NotTriangular] if the input matrix is bigger than 3 x 3 and 
    is not triangular. *)
val determinant_main : t -> float

(** [is_upper_triangular m] returns true if [m] is an upper triangular matrix. 
    Raise: [NotSquare] if the matrix is not square. *)
val is_upper_triangular : t -> bool

(** [is_lower_triangular m] returns true if [m] is a lower triangular matrix. 
    Raise: [NotSquare] if the matrix is not square. *)
val is_lower_triangular : t -> bool

(** [solve m] is the solution to the linear equations in the form of the 
    augmented matrix [m] using row reduction. The left side of augmented matrix
    must be invertible for unique solution. *)
val solve : t -> t

(** [rank m] is the rank of the matrix m. *)
val rank : t -> int

(** [num_sols m] is the number of solutions that an augmented matrix [m] has. It
    can be none, one, or infinite. *)
val num_sols : t -> [> `InfiniteSol | `NoSol | `UniqueSol ]

(** [trace m] is the trace of the matrix [m]. It is the sum of its diagonal 
    elements. Raises: [NotSquare] if [m] is not a square matrix. *)
val trace : t -> float

(** [eigenvals2x2 m] is the pair of eigenvalues for the 2 x 2 matrix [m]. 
    Raises:[NotTwoByTwo] if [m] is not 2 x 2. *)
val eigval2x2 : t -> Complex.t * Complex.t

(** [eigvec2x2 m] is the pair of eigenvectors for the 2x2 matrix [m]. Raises:
    [NotTwoByTwo] if [m] is not 2 x 2. *)
val eigvec2x2 : t -> Complex.t list * Complex.t list

(** [row_space m] is the set of vectors that make up the row space of matrix 
    [m].Each row of the output matrix is one of the vectors that make up the row 
    space basis. *)
val row_space : t -> t

(** [row_space m] is the set of vectors that make up the column space of matrix
    [m].Each row of the output matrix is one of the vectors that make up the
     column space basis. *)
val col_space : t -> t