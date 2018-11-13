open Core


type ast =
  | BinOp of op * ast * ast
  | Lit of int64
[@@deriving show, eq]

and op =
    | Add
  | Mul
[@@deriving show, eq]


(* DSL to make tests easier *)
let ll x = Lit (Int64.of_int x)
let (-+-) x y = BinOp (Add, x, y)
let (-*-) x y = BinOp (Mul, x, y)
let (|+|) x y = (ll x) -+- (ll y)
let (|*|) x y = (ll x) -*- (ll y)



let eval_op (op : op) (v0 : int64) (v1: int64) = Int64.(
    match op with
    | Add -> v0 + v1
    | Mul -> v0 * v1
  )


let rec simple_run = function
  | Lit x -> x
  | BinOp (op, x, y) -> eval_op op (simple_run x) (simple_run y)


(* CPS (continuation passing style) machine.

   Note that this is made simpler than it should be by the fact that
   our operations are all binary. In the general case, a function call
   has a bunch of args and we'd need an enum type with three variants
   to represent args:
     ArgAst: an unevaluated arg
     ArgValue: an evaluated arg
     ArgHole: an arg in the process of being evaluated

   In our little vm, we're able to strip away some complexity, but
   really an EvalOp is like a function call with an ArgHole followed by
   an ArgAst, whereas an ExecOp is like a function call with an ArgValue
   followed by an ArgHole.

   In the general case, we'd exec function calls when there are no
   ArgAsts left, using the exec `value` to fill the hole. Doing this
   with user-defined functions would also require some kind of environment
   model or stack machine setup, but if we just had multi-arity
   builtin functions then `exec`-ing a function would just mean looking
   up the function, and all functions would just take a list of values
   as input.

   (actually the stack machine approach would potentially allow for
   simplification since we could get rid of the ArgHole and ArgValue and just
   push them onto the stack; we'd still need to be pushing an EvalFn object
   with the function itself and all remaining ast args though)
*)

type cps_elem =
  | EvalOp of op * ast
  | ExecOp of op * int64


let cps_run ast =
  let rec eval ast cont = match ast with
    | BinOp (op, x, y) ->
      eval x @@ EvalOp (op, y) :: cont
    | Lit value ->
      exec value cont
  and exec value cont = match cont with
    | [] -> value
    | EvalOp (op, x) :: more ->
      eval x (ExecOp (op, value)::more)
    | ExecOp (op, x) :: more ->
      exec (eval_op op value x) more
  in eval ast []


(* Stack machine

   It's actually pretty easy to generalize this to other types
   of built-in functions. Generalizing it to control structures
   and user-defined functions is trickier though, since that
   would involve ops that aren't just linear but rather an
   array on which we can perform jump operations.

   A simple way to simulate this might be to let the ops be
   a map from int to stack op, allow string labels, and build
   a label map from string to int that jump instructions could
   use. Obviously you'd assemble the lookup table away when
   building a more realistic stack machine.

*)

type stack_machine_op =
  | SmOp of op
  | SmPut of int64


let rec stack_exec stack ops = match ops with
  | [] ->
    (match stack with
     | [value] -> value
     | _ -> failwith "Exited with non-unary stack"
    )
  | SmPut value :: more_ops ->
    stack_exec (value :: stack) more_ops
  | SmOp op :: more_ops ->
    match stack with
    | v0 :: v1 :: more_stack ->
      stack_exec (eval_op op v0 v1 :: more_stack) more_ops
    | _ ->
      failwith @@ "Had stack with fewer than two vals at op " ^ show_op op


let stack_compile the_ast =
  let rec reverse_compile ast ops = match ast with
    | Lit x ->
      SmPut x :: ops
    | BinOp (op, x, y) ->
      let x_ops = reverse_compile x ops in
      let y_ops = reverse_compile y ops in
      SmOp op :: (y_ops @ x_ops @ ops)
  in List.rev @@ reverse_compile the_ast []


let stack_run ast =
  let ops = stack_compile ast in
  stack_exec [] ops
