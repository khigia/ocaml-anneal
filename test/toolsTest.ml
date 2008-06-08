(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

open Tools

let _ = Tests.register "Array.rev" (fun () ->
    let a1 = Array.init 5 (fun idx -> idx) in
    let a2 = Array.rev a1 in
    OUnit.assert_equal a1 [|0;1;2;3;4;|];
    OUnit.assert_equal a2 [|4;3;2;1;0;|];
    ()
)

let _ = Tests.run "Tools test suite"

