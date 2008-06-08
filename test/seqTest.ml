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

let _ = Tests.run "Seq test suite"
