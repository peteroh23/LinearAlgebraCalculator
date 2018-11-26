(** List based vector. *)
type t

(** [empty] is the empty vector. *)
val empty : t

(** [is_empty u] checks whether the vector [u] is empty. *)
val is_empty : t -> bool

(** [cons_v el u] is the vector [u] with [el] added to the front. *)
val cons_v : float -> t -> t

(** [make_v lst] is the vector with elements the same as in [lst]. *)
val make_v : float list -> t

(** [zero_vector n] is a zero vector that has size [n]. *)
val zero_vector : int -> t

(** [dim_v u] is the dimension of vector [u]. Dimension of the empty vector is 0. *)
val dim_v : t -> int

(** [add_v u w] is the element-wise addition of two vectors [u] and [w]. Raises:
    [Invalid_argument] if vectors have different lengths. *)
val add_v : t -> t -> t

(** [scale_v c u] is the scalar multiplication of vector [u] by scalar [c]. *)
val scale_v : float -> t -> t

(** [sub_v u w] is the vector subtraction of [w] from [u]. Raises: [Invalid_argument*)
val sub_v : t -> t -> t

(** [elem_v i u] is the ith element of the vector [u]. Raises: [Invalid_argument]
    if i is <= 0 or [Failure "nth"] if the index is out of bounds. *)
val elem_v : int -> t -> float

(** [dot_v u w] is the inner product of vectors [u] and [w]. *)
val dot_v : t -> t -> float

(** [mag_v u] is the magnitude of the vector [v]. *)
val mag_v : t -> float

(** [proj_v u w] is the projection of vector [w] onto vector [u]. *)
val proj_v : t -> t -> t

(** [hdtl u] splits vector [u] into a pair of its head and tail. Raises: [Failure]
    if the vector is empty. *)
val hdtl : t -> float * t

(** [map f u] applies f to all elements of the vector [u]. *)
val map : (float -> 'a) -> t -> 'a list

(** [rev u] reverses the elements of the vector [u]. *)
val rev : t -> t

(** [has_lead_one u] checks if the vector has a leading one, that is if the 
    first nonzero element is a one. *)
val has_lead_one : t -> bool

(** [to_lst_v u] is the list representation of the vector [u]. *)
val to_lst_v : t -> float list