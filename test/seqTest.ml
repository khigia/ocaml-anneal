(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

let rec _cmp_to_list_end seq lst =
    match lst with
    | [] ->
        OUnit.assert_equal Seq.Nil seq
    | h::[] ->
        OUnit.assert_equal (Some h) (Seq.head seq);
        OUnit.assert_equal None (Seq.head (Seq.tail seq))
    | h::q ->
        OUnit.assert_equal (Some h) (Seq.head seq);
        _cmp_to_list_end (Seq.tail seq) q

let rec _cmp_to_list_cont seq lst =
    match lst with
    | [] ->
        ()
    | h::q ->
        OUnit.assert_equal (Some h) (Seq.head seq);
        _cmp_to_list_cont (Seq.tail seq) q


let _ = Tests.register "Base: head, tail" (fun () ->
    let s = Seq.Cons(
        1,
        lazy (Seq.Cons(
            2,
            lazy Seq.Nil
        ))
    ) in
    _cmp_to_list_end s [1;2;]
)

let _ = Tests.register "Base: head_exn" (fun () ->
    let s = Seq.Nil in
    OUnit.assert_raises Seq.EmptySeq (fun () -> Seq.head_exn s)
)

let _ = Tests.register "Helper: of_list" (fun () ->
    let l = [1;2;3;] in
    let s = Seq.of_list l in
    _cmp_to_list_end s l
)

let _ = Tests.register "Helper: of_serie" (fun () ->
    let fn = fun x -> x + 1 in
    let s = Seq.of_serie fn 0 in
    _cmp_to_list_cont s [0;1;2;3;4;5;]
)

let _ = Tests.register "map" (fun () ->
    let fn = fun x -> x + 1 in
    let s = Seq.of_serie fn 0 in
    let s2 = Seq.map (fun x -> x * 2) s in
    _cmp_to_list_cont s2 [0;2;4;6;]
)

let _ = Tests.register "gmap" (fun () ->
    let s1 = Seq.of_list [1;2;3;] in
    let s2 = Seq.of_list [10;20;30;] in
    let fn heads = match heads with |x::y::[] -> x + y |_ -> failwith "error" in
    _cmp_to_list_cont
        (Seq.gmap_exn fn [s1;s2;])
        [11; 22; 33;];
    let s3 = Seq.of_list [1;2;3;4;] in
    let err = Seq.gmap_exn fn [s3;s2;] in
    OUnit.assert_raises Seq.EmptySeq (fun () ->
        _cmp_to_list_cont err [11; 22; 33;]
    )
)

let _ = Tests.register "iter" (fun () ->
    let l = [1;2;3;] in
    let s = Seq.of_list l in
    let acc = ref [] in
    let fn = fun x -> acc := x :: !acc in
    Seq.iter fn s;
    OUnit.assert_equal !acc (List.rev l)
)

let _ = Tests.register "filter" (fun () ->
    let s = Seq.of_list [0;1;2;3;4;5;6;7;8;9;] in
    let f = Seq.filter (fun x -> x mod 2 = 0) s in
    _cmp_to_list_end f [0;2;4;6;8;]
)

let _ = Tests.run "Seq test suite"
