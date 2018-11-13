open Core


module type BaseAbstractStack = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val pop  : 'a t -> 'a t
  val peek : 'a t -> 'a option

end

module type AbstractStack = sig
  include BaseAbstractStack

  val format :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit
end

module MkAbstractStack ( M : BaseAbstractStack ) = struct

  include M

  let rec for_each
      (f: 'a -> unit)
      (s: 'a t)
    : unit =
    match peek s with
    | None -> ()
    | Some x ->
      f x;
      for_each f @@ pop s

  (* I'm not sure how to embed this into the source, but you want
     to use the top-level directive:

     As far as I can tell, there actually is no way to call such a
     directive from the source code, which seems kind of awful. But
     on the other hand, you can define these things for a project
     in a `.ocamlinit` file; I added one to this project
  *)
  let format
      (fmt_elt : Format.formatter -> 'a -> unit)
      (fmt : Format.formatter)
      (s: 'a t)
    : unit =
    let
      f = (fun elt -> Format.fprintf fmt "%a; " fmt_elt elt)
    in
    Format.pp_print_string fmt "Stack {";
    for_each f s;
    Format.pp_print_string fmt "}";
end


module ListStackImpl = MkAbstractStack ( struct
    type 'a t = 'a list

    let empty = []
    let is_empty = function
      | [] -> true
      | _ -> false
    let push x st = x :: st
    let pop = function
      | [] -> []
      | _::xs -> xs
    let peek = function
      | [] -> None
      | x::_ -> Some(x)
  end
  )


(* We can export an "exposed" version where the type is
   visible, for tests and such *)
module ListStackExposed : (AbstractStack with type 'a t = 'a list) =
  ListStackImpl

(* and a non-exposed version for users where the type
   isn't visible. The important thing is that by adding a module
   type T to a module M, we hide everything in M that isn't part of
   T. *)
module ListStack : AbstractStack =
  ListStackImpl


module CustomStackImpl = MkAbstractStack ( struct
    type 'a t =
      | Empty
      | Entry of 'a * 'a t

    let empty = Empty
    let is_empty s = s = Empty
    let push x s = Entry (x, s)
    let peek = function
      | Empty -> None
      | Entry(x,_) -> Some(x)
    let pop = function
      | Empty -> Empty
      | Entry(_,s) -> s
  end
  )


module CustomStack : AbstractStack = CustomStackImpl


(* TODO
   - Chapter 4
     - Do the linear algebra exercises
   - Chapter 5
     - Study the TwoListQueue implementation
     - Make a binary search tree dict implementation
     - Study the implementation of Jane Street Core's Map;
       understand what happens when we call Make.
     - Make a nice Rational implementation, and do the
       Ring / Field exercise (end of Chapter 4)
   - Chapter 6
     - Think a little bit more about interfaces, representation
       abstractions, and representation invariants. Maybe tackle
       a few of the exercises.
   - Chapter 7
     - Learn about QuickCheck and randomized testing in ocaml
   - Chapter 8
     - Study in depth
   - Chapter 9
     - Study in depth
*)
