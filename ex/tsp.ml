open Tools

let shuffle_seq size =
    let indexes = Array.init size (fun idx -> idx) in
    let rec _shuffle arr len pos =
        if pos < len
        then
            let idx = Random.int (len - pos) in
            let cur = arr.(pos + idx) in
            arr.(pos + idx) <- arr.(pos);
            Seq.Cons(cur, lazy (_shuffle arr len (pos + 1)))
        else
            Seq.Nil
    in
    _shuffle indexes size 0

let pair_shuffle_seq size =
    let s1 = shuffle_seq size in
    let s2 = shuffle_seq size in
    Seq.cart [s1; s2;]

let reversed_section tour =
    let len = Array.length tour in
    let rtour = Array.rev tour in
    pair_shuffle_seq len
    |> Seq.map (fun l -> match l with |x::y::[] -> (x,y) | _ -> failwith "error")
    |> Seq.filter (fun (i,j) -> i <> j)
    |> Seq.map (fun (i,j) ->
        if i < j
        then
            begin
            let sol = Array.copy tour in
            Array.blit rtour (len - j - 1) sol i (j + 1 - i);
            sol
            end
        else
            begin
            let sol = Array.copy rtour in
            Array.blit tour j sol (len - i - 1) (i + 1 - j);
            sol
            end
    )
    |> Seq.filter (fun sol -> sol <> tour)

let kirkpatrick_seq alpha initial =
    let kirkpatrick alpha temperature = temperature *. alpha in
    Seq.of_serie (kirkpatrick alpha) initial

