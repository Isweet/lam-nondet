open Core      

type t =
  | Base of Bool.t
  | Nondet
  | If of t * t * t
  | Var of String.t
  | Lam of String.t * t
  | App of t * t

val eval : t -> t List.t
            
