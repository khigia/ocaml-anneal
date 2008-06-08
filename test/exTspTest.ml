(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

open Tools

let _ = Tests.register "reversed_section (permutations)" (fun () ->
    Random.self_init ();
    let base = [| 1; 2; 3;|] in
    let len = Array.length base in
    let perms = 
        Tsp.reversed_section base
        |> Seq.to_list
        |> List.sort compare
    in
    (* number of perms = n * (n-1) - 1 ... -1 coz input not in input! *)
    OUnit.assert_equal (List.length perms) (len * (len - 1) - 1);
    OUnit.assert_equal perms [
        [|1;3;2;|];
        [|2;1;3;|];
        [|2;3;1;|];
        [|3;1;2;|];
        [|3;2;1;|];
    ]
)

let _ = Tests.run "TSP test suite"


