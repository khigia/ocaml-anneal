open Tools
open Tsp

let objective arr =
    Array.fold_left (fun (i,sum) e -> (i + 1, sum + e * i)) (1, 0) arr
    |> snd
    |> float

let _ =
    let init = [|1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;|] in
    let cooling = kirkpatrick_seq 0.9999 10.0 in
    let (n, sol, score) = Anneal.optimize init objective reversed_section 500000 cooling in
    Printf.printf "%d evaluations; score=%f; sol=\n" n score;
    Array.iter (Printf.printf "%d ") sol;
    print_newline ()

