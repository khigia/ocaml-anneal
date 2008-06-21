let rec cmp_to_list_end seq lst =
    match lst with
    | [] ->
        OUnit.assert_equal Seq.Nil seq
    | h::[] ->
        OUnit.assert_equal (Some h) (Seq.head seq);
        OUnit.assert_equal None (Seq.head (Seq.tail seq))
    | h::q ->
        OUnit.assert_equal (Some h) (Seq.head seq);
        cmp_to_list_end (Seq.tail seq) q

let rec cmp_to_list_cont seq lst =
    match lst with
    | [] ->
        ()
    | h::q ->
        OUnit.assert_equal (Some h) (Seq.head seq);
        cmp_to_list_cont (Seq.tail seq) q

