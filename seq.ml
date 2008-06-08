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


