module type BaseMonad = sig
  type 'a t

  val pure: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end


module Monad(M: BaseMonad) = struct
  include M
  let return = pure
  let (>>=) = bind
  let (>>) m0 m1 = m0 >>= fun _ -> m1
end


module BaseMaybeT(M: BaseMonad) = Monad ( struct
  type 'a t = ('a option) M.t

  let pure x = M.pure (Some x)

  let bind m f =
    M.bind m (fun x -> match x with
        | None -> M.pure None
        | Some xx -> f xx
      )
end )


module type State = sig type t end


module BaseStateT (M: BaseMonad) (S: State) = struct
  type 'a t = S.t -> ('a * S.t) M.t

  let pure x = fun (s: S.t) -> M.pure (x, s)

  let bind (m: 'a t) (f: 'a -> 'b t) = fun (s: S.t) ->
    let m0 = m s in
    M.bind m0 (fun (x0, s0) -> f x0 s0)
end



