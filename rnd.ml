let dichotomic_walk_seq arr =
    let len = Array.length arr in
    let s = Seq.dichotomy_int 0 len in
    Seq.map (fun idx -> arr.(idx)) (Seq.push_front 0 s)
