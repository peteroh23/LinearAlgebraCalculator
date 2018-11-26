exception Malformed
exception Empty

type operation = 
  | Quiz
  | Add
  | Subtract
  | Scale
  | Multiply
  | Transpose
  | Inverse
  | Rref
  | Solve
  | Rank
  | NumSols
  | Trace
  | Determinant
  | Rowspace
  | Colspace
  | Eigvals
  | Eigvects
  | Assign
  | Get
  | Save
  | Help of string
  | Quit

val parse : string -> operation