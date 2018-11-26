exception Empty
exception Malformed

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


let to_list_str str = 
  List.filter (fun el -> el <> "") (String.split_on_char ' ' str)

let parse str = match to_list_str str with
    [] -> raise Empty
  | h::[] -> (match h with
      | "quiz" -> Quiz
      | "add" -> Add
      | "subtract" -> Subtract
      | "scale" -> Scale
      | "multiply" -> Multiply
      | "transpose" -> Transpose
      | "inverse" -> Inverse
      | "rref" -> Rref
      | "solve" -> Solve
      | "rank" -> Rank
      | "#solutions" -> NumSols
      | "trace" -> Trace
      | "eigenvalues" -> Eigvals
      | "eigenvectors" -> Eigvects
      | "determinant" -> Determinant
      | "rowspace" -> Rowspace
      | "columnspace" -> Colspace
      | "assign" -> Assign
      | "get" -> Get
      | "save" -> Save
      | "quit" -> Quit
      | "help" -> Help ""
      | "add?" -> Help "add"
      | "subtract?" -> Help "subtract"
      | "scale?" -> Help "scale"
      | "multiply?" -> Help "multiply"
      | "transpose?" -> Help "transpose"
      | "inverse?" -> Help "inverse"
      | "rref?" -> Help "rref"
      | "solve?" -> Help "solve"
      | "rank?" -> Help "rank"
      | "#solutions?" -> Help "#solutions"
      | "trace?" -> Help "trace"
      | "eigenvalues?" -> Help "eigenvalues"
      | "eigenvectors?" -> Help "eigenvectors"
      | "determinant?" -> Help "determinant"
      | "rowspace?" -> Help "rowspace"
      | "columnspace?" -> Help "columnspace"
      | "assign?" -> Help "assign"
      | "get?" -> Help "get"
      | "save?" -> Help "save"
      | "help?" -> Help "help"
      | "quiz?" -> Help "quiz"
      | "quit?" -> Help "quit"
      | s -> raise Malformed)
  | s -> raise Malformed


