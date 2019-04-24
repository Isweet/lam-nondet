open Core

type t =
  | Base of Bool.t
  | Nondet
  | If of t * t * t
  | Var of String.t
  | Lam of String.t * t
  | App of t * t
[@@deriving compare]

let rec run ctx e =
  let open List.Let_syntax in
  match e with
  | Base _ -> return e
  | Nondet -> [ Base true ; Base false ]
  | If (e1, e2, e3) ->
    let%bind guard = run ctx e1 in
    (match guard with
     | Base guard -> if guard then run ctx e2 else run ctx e3
     | _ -> raise (Failure "Non-boolean expression in guard of if-stmt"))
  | Var x -> Map.find_exn ctx x
  | Lam _ -> return e
  | App (e1, e2) ->
    let%bind f = run ctx e1 in
    (match f with
     | Lam (x, e) ->
       run (Map.set ctx ~key:x ~data:(run ctx e2)) e
     | _ -> raise (Failure "Non-function expression in application"))

let rec run2 ctx e =
  let open List.Let_syntax in
  match e with
  | Base _ -> return e
  | Nondet -> [ Base true ; Base false ]
  | If (e1, e2, e3) ->
    run ctx e1 >>= fun guard ->
    (match guard with
     | Base guard -> if guard then run ctx e2 else run ctx e3
     | _ -> raise (Failure "Non-boolean expression in guard of if-stmt"))
  | Var x -> Map.find_exn ctx x
  | Lam _ -> return e
  | App (e1, e2) ->
    run ctx e1 >>= fun f ->
    (match f with
     | Lam (x, e) ->
       run (Map.set ctx ~key:x ~data:(run ctx e2)) e
     | _ -> raise (Failure "Non-function expression in application"))

let rec run3 ctx e =
  match e with
  | Base _ -> [ e ]
  | Nondet -> [ Base true ; Base false ]
  | If (e1, e2, e3) ->
    List.concat_map
      ~f:(fun guard ->
          (match guard with
           | Base guard -> if guard then run ctx e2 else run ctx e3
           | _ -> raise (Failure "Non-boolean expression in guard of if-stmt")))
      (run ctx e1)
  | Var x -> Map.find_exn ctx x
  | Lam _ -> [ e ]
  | App (e1, e2) ->
    List.concat_map
      ~f:(fun f ->
          (match f with
           | Lam (x, e) ->
             run (Map.set ctx ~key:x ~data:(run ctx e2)) e
           | _ -> raise (Failure "Non-function expression in application")))
      (run ctx e1)

let rec run4 ctx e =
  match e with
  | Base _ -> [ e ]
  | Nondet -> [ Base true ; Base false ]
  | If (e1, e2, e3) ->
    List.map
      ~f:(fun guard ->
          (match guard with
           | Base guard -> if guard then run ctx e2 else run ctx e3
           | _ -> raise (Failure "Non-boolean expression in guard of if-stmt")))
      (run ctx e1) |> List.concat
  | Var x -> Map.find_exn ctx x
  | Lam _ -> [ e ]
  | App (e1, e2) ->
    List.map
      ~f:(fun f ->
          (match f with
           | Lam (x, e) ->
             run (Map.set ctx ~key:x ~data:(run ctx e2)) e
           | _ -> raise (Failure "Non-function expression in application")))
      (run ctx e1) |> List.concat

let rec run5 ctx e =
  match e with
  | Base _ -> [ e ]
  | Nondet -> [ Base true ; Base false ]
  | If (e1, e2, e3) ->
    List.concat
      (List.map
         ~f:(fun guard ->
             (match guard with
              | Base guard -> if guard then run ctx e2 else run ctx e3
              | _ -> raise (Failure "Non-boolean expression in guard of if-stmt")))
         (run ctx e1))
  | Var x -> Map.find_exn ctx x
  | Lam _ -> [ e ]
  | App (e1, e2) ->
    List.concat
      (List.map
         ~f:(fun f ->
             (match f with
              | Lam (x, e) ->
                run (Map.set ctx ~key:x ~data:(run ctx e2)) e
              | _ -> raise (Failure "Non-function expression in application")))
         (run ctx e1))

(* run = run2 = run3 = run4 = run5 *)

let eval e = List.dedup_and_sort ~compare (run (Map.empty (module String)) e)
