open Tools
open Tsp

let positions = Hashtbl.create 1000
let distances = Hashtbl.create 1000

let locate pt =
    Hashtbl.find positions pt

let distance p1 p2 =
    let key = if p1 < p2 then (p1, p2) else (p2, p1) in
    try
        Hashtbl.find distances key
    with
        | Not_found ->
            (* TODO from p1 to position p1*)
            let loc1 = locate p1 in
            let loc2 = locate p2 in
            let d = Tsp.LatLon.distance loc1 loc2 in
            Hashtbl.replace distances key d;
            d

let read_positions buf =
    let hdr = "Reading locations: " in
    let rec _read_loop () =
        begin
            try
                Scanf.bscanf buf "%d %f %f%s@\n" (fun pt lat lon _ ->
                    Hashtbl.replace positions pt (lat, lon)
                )
            with
                | Scanf.Scan_failure msg ->
                    Scanf.bscanf buf "%s@\n" (fun s ->
                        Printf.eprintf "%sskiping line: %s\n" hdr s
                    )
        end;
        _read_loop ()
    in
    try _read_loop () with | End_of_file ->
        Printf.eprintf "%sdone.\n" hdr;
        flush_all ()

let path_length pts =
    Array.fold_left 
        (fun (cur, acc) pt -> (pt, acc +. (distance cur pt)))
        (pts.(0), 0.)
        pts
    |> snd

    
    (*
let objective arr =
    Array.fold_left (fun (i,sum) e -> (i + 1, sum + e * i)) (1, 0) arr
    |> snd
    |> float

let _ =
    let init = [|1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;|] in
    let cooling = Anneal.kirkpatrick_seq 0.9999 10.0 in
    let (n, sol, score) = Anneal.optimize init objective reversed_section 500000 cooling in
    Printf.printf "%d evaluations; score=%f; sol=\n" n score;
    Array.iter (Printf.printf "%d ") sol;
    print_newline ()
    *)

let _ =
    let posfn = ref "coords.txt" in
    let usage = Printf.sprintf "Usage: %s [-help] [-coords filename]" Sys.argv.(0) in
    Arg.parse
        [
            ("-coords",
                Arg.String ((:=) posfn),
                (Printf.sprintf "filename: locations file name; default to \"%s\" (file format: n lat lon)" !posfn)
            );
        ]
        ignore
        usage;
    try
        let buf = Scanf.Scanning.from_file !posfn in
        let _ = read_positions buf in
        let doit path =
            let d = path_length path in
            Printf.printf "Path:";
            Array.iter (Printf.printf " %d") path;
            Printf.printf ": %f\n" d;
            let cooling = Anneal.kirkpatrick_seq 0.9999 10.0 in
            let (n, sol, score) = Anneal.optimize path (fun p -> -. path_length p) reversed_section 5000 cooling in
            Printf.printf "  Optimization: %d evaluations; score=%f; sol=" n score;
            Array.iter (Printf.printf "%d ") sol;
            print_newline ()
        in
        (*
        let _ = doit [|1; 1;|] in (* TODO something wrong ... infinite loop ... probably in solution generation! *)
        let _ = doit [|1; 2;|] in
        let _ = doit [|1; 3;|] in
        let _ = doit [|2; 3;|] in
        let _ = doit [|3; 2;|] in
        *)
        let _ = doit [|1; 1; 2;|] in
        let _ = doit [|1; 2; 3;|] in
        ()
    with
        exn ->
            Printf.printf "ERROR:%s\n%s\n" (Printexc.to_string exn) usage;


