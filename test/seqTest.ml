(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

let _ = Tests.register "Base: head, tail" (fun () ->
    let s = Seq.Cons(
        1,
        lazy (Seq.Cons(
            2,
            lazy Seq.Nil
        ))
    ) in
    OUnit.assert_equal (Some 1) (Seq.head s);
    OUnit.assert_equal (Some 2) (Seq.head (Seq.tail s));
    OUnit.assert_equal Seq.Nil (Seq.tail (Seq.tail s));
    OUnit.assert_equal None (Seq.head (Seq.tail (Seq.tail s)));
    ()
)

let _ = Tests.run "Seq test suite"
