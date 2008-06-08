(* Base *)

exception EmptySeq

type 'a t =
    | Nil
    | Cons of 'a * 'a t lazy_t

let head seq =
    match seq with
    | Nil ->
        None
    | Cons(h, q) ->
        Some h

let head_exn seq =
    match seq with
    | Nil ->
        raise EmptySeq
    | Cons(h, q) ->
        h

let tail seq =
    match seq with
    | Nil ->
        Nil
    | Cons(h, q) ->
        Lazy.force q


(* Transformation helpers *)

let rec of_list lst =
    match lst with
    | h :: q ->
        Cons(h, lazy (of_list q))
    | [] ->
        Nil

let to_list seq =
    let rec _to_list seq acc =
        match head seq with
        | Some h ->
            _to_list (tail seq) (h::acc)
        | None ->
            List.rev acc
    in
    _to_list seq []

let rec of_serie fn n0 =
    Cons(n0, lazy (of_serie fn (fn n0)))


(* Manipulation *)

let rec map fn seq =
    (* seq is last arg such that forward op can be used *)
    match head seq with
    | None ->
        Nil
    | Some e ->
        Cons(fn e, lazy (map fn (tail seq)))

let rec gmap_exn fn seqs =
    match seqs with
    | Nil :: _ ->
        Nil
    | _ ->
        let heads = List.map head_exn seqs in
        Cons(fn heads, lazy (gmap_exn fn (List.map tail seqs)))

let rec iter fn seq =
    match head seq with
    | None ->
        ()
    | Some e ->
        let _ = fn e in
        iter fn (tail seq)

let rec filter pred seq =
    match head seq with
    | None ->
        Nil
    | Some h ->
        if pred h
        then
            Cons(h, lazy (filter pred (tail seq) ))
        else
            filter pred (tail seq)

let rec concat seqs =
    match seqs with
    | h :: a ->
        begin
        match head h with
        | None ->
            concat a
        | Some e ->
            Cons(e, lazy (concat ((tail h) :: a)))
        end
    | [] ->
        Nil

let rec cart seqs =
    match seqs with
    | [] ->
        Nil
    | h :: [] ->
        map (fun e -> [e;]) h
    | h :: a ->
        match head h with
        | None ->
            Nil
        | Some e ->
            concat [
                (map (fun c -> e :: c) (cart a));
                (cart ((tail h) :: a));
            ]
