let dichotomic_seq arr =
    let len = Array.length arr in
    let s = Seq.dichotomy_int 0 len in
    Seq.map (fun idx -> arr.(idx)) (Seq.push_front 0 s)

let pair_permutation_seq x y =
    let xs = Seq.of_array x in
    let ys = Seq.of_array y in
    let s = Seq.cart [xs;ys;] in
    Seq.map (fun [i;j;] -> (i,j)) s

let pair_permutation_random_seq x y =
    let diag_shuffle_seq a order =
        let l = Array.length a in
        let lindexes = Seq.range_int 1 l in
        let lower_diag_i = Seq.map (fun i -> Seq.range_int i l) lindexes in
        let uindexes = Seq.range_int (l-1) 0 ~step:(-1) in
        let upper_diag_i = Seq.map (fun i -> Seq.range_int 0 i) uindexes in
        let diags =
            if order > 0
            then Seq.combine lower_diag_i upper_diag_i
            else Seq.combine upper_diag_i lower_diag_i
        in
        let diags = Seq.push_front (Seq.range_int 0 l) diags in
        let indexes = Seq.concat diags in
        let sh = Rnd.Shuffle.create a in
        Seq.map (Rnd.Shuffle.get sh) indexes
    in
    let sx = diag_shuffle_seq x 1 in
    let sy = diag_shuffle_seq y (-1) in
    Seq.gmap_exn (fun [i;j;] -> (i,j)) [sx; sy;]


