(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

let _ = Tests.register "Shuffle: create" (fun () ->
    let creator () =
        let _ = Rnd.Shuffle.create [|1;2;3;4;5;|] in
        true
    in
    OUnit.assert_bool "cannot create shuffle" (creator ())
)

let _ = Tests.register "Shuffle: get" (fun () ->
    let a = [|1;2;3;4;5;|] in
    let l = Array.length a in
    let sh = Rnd.Shuffle.create a in
    let r = Array.make l 0 in
    for idx = 0 to (l - 1) do
        r.(idx) <- Rnd.Shuffle.get sh idx
    done;
    Array.sort compare r;
    OUnit.assert_equal a r;
    OUnit.assert_raises (Failure "index error") (fun () -> Rnd.Shuffle.get sh l);
    OUnit.assert_raises (Failure "index error") (fun () -> Rnd.Shuffle.get sh (l + 1));
    ()
)

let _ = Tests.run "Rnd test suite"

