(*
OUnit examples:
    OUnit.assert_bool "reason if failure" (x == 2)
    OUnit.assert_equal x y
Tests examples:
    Tests.register "my feature" (fun () -> ... some code with OUnit assertions)
    Tests.run "Name of the suite"
*)

module IntPairSet = struct

    include Set.Make (struct
        type t = int * int
        let compare p1 p2 = compare p1 p2
    end)

    let of_list l =
        List.fold_left (fun acc e -> add e acc) empty l

end (* module IntPairSet *)

let _ = Random.self_init ()

let _ = Tests.register "dichotomic_seq" (fun () ->
    let s = Walks.dichotomic_seq [|0;1;2;3;4;5;6;7;8;9;|] in
    SeqUtil.cmp_to_list_end s [0;5;2;7;1;6;3;8;4;9;]
)

let _ = Tests.register "pair_permutation_seq" (fun () ->
    let s = Walks.pair_permutation_seq [|1;2;3;|] [|1;2;3;|] in
    SeqUtil.cmp_to_list_end s [
        (1,1); (1,2); (1,3);
        (2,1); (2,2); (2,3);
        (3,1); (3,2); (3,3);
    ]
)

let _ = Tests.register "pair_permutation_random_seq" (fun () ->
    let s = Walks.pair_permutation_random_seq [|1;2;3;|] [|1;2;3;|] in
    let l = Seq.to_list s in
    let set1 = IntPairSet.of_list l in
    let set2 = IntPairSet.of_list [
        (1,1); (1,2); (1,3);
        (2,1); (2,2); (2,3);
        (3,1); (3,2); (3,3);
    ] in
    OUnit.assert_bool
        "seq does not contains all possible pairs"
        (IntPairSet.equal set1 set2)
)

let _ = Tests.run "Walks test suite"


