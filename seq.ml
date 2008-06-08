(* Base *)

type 'a t =
    | Nil
    | Cons of 'a * 'a t lazy_t

let head seq =
    match seq with
    | Nil ->
        None
    | Cons(h, q) ->
        Some h

let tail seq =
    match seq with
    | Nil ->
        Nil
    | Cons(h, q) ->
        Lazy.force q


(* Helper *)

let rec of_list lst =
    match lst with
    | h :: q ->
        Cons(h, lazy (of_list q))
    | [] ->
        Nil

let rec of_serie fn n0 =
    Cons(n0, lazy (of_serie fn (fn n0)))


