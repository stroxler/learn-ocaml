open Core


(* Notes

   1. Think about whether there's a way to use the Haskell
      MonadX (MonadReader, MonadWriter, MonadIO) pattern from
      mtl in the context of ocaml. Thus far I'm not seeing an
      easy way to do it.

   2. Think about whether to use the Make2 interface from
      jane street core, rather than what I'm doing now. At the
      moment I'm kind of inclined to leave things as they are.

   3. Study existing MTL implementations...
      - This one is unmaintained, but has some nice examples:
        https://github.com/rgrinberg/ocaml-mtl/
      - There's a pretty serious monad library, including transformers,
        embedded in the binary analysis project from CMU
        https://discuss.ocaml.org/t/ann-monads-the-missing-monad-transformers-library/830/4
        http://binaryanalysisplatform.github.io/bap/api/v1.3.0/Monads.Std.html
        https://github.com/BinaryAnalysisPlatform/bap/tree/master/lib/monads

*)

(* Turns a monad basic's [bind] (a function), [return] (a function), and
   [map] (an enum which is either [`Define_using_bind] or [`Custom func]
   into a callable version of [map] for the [Monad.Basic].

   Useful for defining a custom [map] implementation in our transformers.
*)
let map_from_basic bind_func return_func = function
  | `Custom func -> func
  | `Define_using_bind ->
    let func m ~f = bind_func m ~f:(fun x -> return_func (f x))
    in func


module Identity: Monad.S with type 'a t = 'a = struct

  type 'a t = 'a

  include Monad.Make ( struct
      type 'a t = 'a
      let return x = x
      let bind m ~f = f m
      let map = `Custom (fun m ~f -> f m)
    end )
end


(* Helper types

   Concrete is needed for several monads that have an extra
   type argument, e.g. State, Reader, Cont

   Monoid is needed for Writer
*)

module type Concrete = sig
  type t
end


module type Monoid = sig
  type t
  val mempty: t
  val mappend: t -> t -> t
end


module ListMonoid(A: Concrete) = struct
  type t = A.t list
  let mempty = []
  let mappend = (@)
end


module CounterMonoid = struct
  type 'a t = Int.t
  let mempty = 0
  let mappend = (+)
end



module OptionT(M: Monad.Basic)
  : Monad.S with type 'a t = 'a Option.t M.t = struct

  type 'a t = 'a Option.t M.t

  include Monad.Make(
    struct
      type 'a t = 'a Option.t M.t

      let return x = M.return (Some x)

      let bind m ~f = M.bind m ~f:(fun x -> match x with
          | Some(v) -> f v
          | None -> M.return None
        )


      let map =
        `Define_using_bind
        (* For reasons unclear to me, I cannot resolve a type error here
         * no matter what I do: it says the type is
         * `Custom of '_a t -> ('_a -> '_b) -> '_b t
         * which does not match
         *   `Custom of 'a t -> ('a -> 'b) -> 'b t
         * which makes no sense to me, these are the same up to names
         * for the types.
         *
           let map_M = map_from_basic M.bind M.return M.map in
           `Custom (
           fun (m: 'a t) ~(f: 'a -> 'b): 'b t ->
             map_M m ~f:(fun opt -> Option.map ~f opt)
           )
        *)
    end )


  let lift (m: 'a M.t) : 'a t =
    M.bind m ~f:(
      fun x -> M.return @@ Some x
    )

  (* Todo: add the mtl goodies here *)
end



module EitherT (M: Monad.Basic) (E: Concrete)
  : Monad.S with type 'a t = ('a, E.t) Either.t M.t = struct

  type 'a t = ('a, E.t) Either.t M.t

  include Monad.Make(
    struct
      type 'a t = ('a, E.t) Either.t M.t

      let return x = M.return (First x)

      let bind m ~f = M.bind m ~f:(fun x -> match x with
          | First(v) -> f v
          | Second(e) -> M.return @@ Second(e)
        )

      let map = `Define_using_bind

    end )


  let lift (m: 'a M.t) : 'a t =
    M.bind m ~f:(
      fun a -> M.return @@ Either.First a
    )


  let throwSecond (e: E.t) : 'a t =
    M.return @@ Either.Second e

  let catchSecond m handler =
    M.bind m ~f:(
      fun x -> match x with
        | Second e -> handler e
        | _first -> m
    )

  let liftEither eith = M.return eith

end


module StateT (M: Monad.Basic) (S: Concrete)
  : Monad.S with type 'a t = (S.t -> ('a * S.t) M.t) = struct

  type 'a t = (S.t -> ('a * S.t) M.t)

  include Monad.Make(
    struct

      type 'a t = (S.t -> ('a * S.t) M.t)

      let return x = fun s -> M.return (x, s)

      let bind m ~f = fun s ->
        let ran0 = m s in
        M.bind ran0 ~f:(fun (x0, s0) ->
            let m1 = f x0 in
            m1 s0
          )

      let map = `Define_using_bind

    end )


  let lift (m: 'a M.t) : 'a t =
    fun s -> M.bind m ~f:(
         fun a -> M.return (a, s)
       )

end


module WriterT (M: Monad.Basic) (W: Monoid)
  : Monad.S with type 'a t = ('a * W.t) M.t = struct

  type 'a t = ('a * W.t) M.t

  include Monad.Make(
    struct

      type 'a t = ('a * W.t) M.t

      let return x = M.return (x, W.mempty)

      let bind m ~f =
        M.bind m ~f:(
          fun (x0, w0) ->
            let m1 = f x0 in
            M.bind m1 ~f:(
              fun (x1, w1) ->
                M.return (x1, W.mappend w0 w1)
            )
        )

      let map = `Define_using_bind

    end )


  let lift (m: 'a M.t) : 'a t =
    M.bind m ~f:(
      fun a -> M.return (a, W.mempty)
    )

end



module ReaderT (M: Monad.Basic) (R: Concrete)
  : Monad.S with type 'a t = R.t -> 'a M.t = struct

  type 'a t = R.t -> 'a M.t

  include Monad.Make(
    struct
      type 'a t = R.t -> 'a M.t

      let return x = fun _ -> M.return x

      let bind m ~f = (
        fun (r: R.t) ->
          M.bind (m r) ~f:(
            fun x ->
              let m1 = f x in
              m1 r
          )
      )

      let map = `Define_using_bind

    end )

  let lift (m: 'a M.t) : 'a t =
    (fun r -> m)

end


module ContT (M: Monad.Basic) (R: Concrete)
  : Monad.S with type 'a t = ('a -> R.t M.t) -> R.t M.t = struct

  type 'a t = ('a -> R.t M.t) -> R.t M.t

  include Monad.Make(
    struct
      type 'a t = ('a -> R.t M.t) -> R.t M.t

      let return x = fun cb -> cb x

      (* The type annotations are kind of verbose, but
         also help clarify what's going on here *)
      let bind (m: 'a t) ~(f: 'a -> 'b t) =
        fun (cb_on_b: 'b -> R.t M.t): R.t M.t ->
          m (fun (a: 'a): R.t M.t ->
              let run1: ('b -> R.t M.t) -> R.t M.t = f a in
              run1 cb_on_b
            )

      let map = `Define_using_bind

    end )

  (* Note that this is the only lift implementation
     that actually *require* bind *)
  let lift (m: 'a M.t) : 'a t =
    fun cb -> M.bind m ~f:cb

end
