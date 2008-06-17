open Tools

let shuffle_seq size =
    let indexes = Array.init size (fun idx -> idx) in
    let rec _shuffle arr len pos =
        if pos < len
        then
            let idx = Random.int (len - pos) in
            let cur = arr.(pos + idx) in
            arr.(pos + idx) <- arr.(pos);
            Seq.Cons(lazy cur, lazy (_shuffle arr len (pos + 1)))
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


module LatLon = struct

    let pi = acos (-. 1.)

    let radians deg =
        deg *. pi /. 180.

    let degrees rad =
        180. *. rad /. pi

    let _haversine (lat1d, lon1d) (lat2d, lon2d) =
        let lat1 = radians lat1d in
        let lon1 = radians lon1d in
        let lat2 = radians lat2d in
        let lon2 = radians lon2d in
        let dLat_2_sin = sin ( (lat2 -. lat1) /. 2. ) in
        let dLon_2_sin = sin ( (lon2 -. lon1) /. 2. ) in
        let a =
            dLat_2_sin *. dLat_2_sin
            +. (cos lat1) *. (cos lat2) *. dLon_2_sin *. dLon_2_sin
        in
        let c = 2. *. (atan2 (sqrt a) (sqrt (1. -. a))) in
        6371000. *. c

    let distance p1 p2 =
        if p1 = p2 then 0. else _haversine p1 p2

end (* module LatLon *)
