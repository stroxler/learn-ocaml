open Core

type ast =
  | Int of int
  | Binop of bop * ast * ast
  [@@deriving show,eq,sexp]

and bop =
  | Add
  | Sub
  | Mul
  | Div
  [@@deriving show,eq,sexp]
