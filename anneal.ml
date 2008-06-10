
let kirkpatrick_seq alpha initial =
    let kirkpatrick alpha temperature = temperature *. alpha in
    Seq.of_serie (kirkpatrick alpha) initial


type 'a sol =
    | Final of 'a state
    | Intermediate of 'a state
and 'a state = 'a * float * 'a * float * int

let acceptable temperature curS nextS =
    let p = if nextS > curS
        then 1.0
        else exp( -1.0 *. (abs_float ((nextS -. curS) /. temperature)) )
    in
    Random.float 1.0 < p

let rec explore temperature (current, currentScore) (best, bestScore) evalN evalMax solutions objective =
    (* seq_find ... *)
    match Seq.head solutions with
    | None ->
        Intermediate(current, currentScore, best, bestScore, evalN)
    | Some next ->
        if not (evalN < evalMax)
        then Final(current, currentScore, best, bestScore, evalN)
        else
            let nextScore = objective next in
            let newBest, newBestScore = if nextScore > bestScore
                then next, nextScore
                else best, bestScore
            in
            if acceptable temperature currentScore nextScore
            then
                Intermediate(next, nextScore, newBest, newBestScore, evalN+1)
            else
                explore temperature (current, currentScore) (newBest, newBestScore) (evalN+1) evalMax (Seq.tail solutions) objective
                
let rec cooling coolingSeq (current, currentScore) (best,bestScore) evalN evalMax explorer objectiveF =
    match Seq.head coolingSeq with
    | None ->
        (evalN, best, bestScore)
    | Some temperature ->
        begin
        let solutions = explorer current in
        match explore temperature (current, currentScore) (best, bestScore) evalN evalMax solutions objectiveF with
        | Intermediate(c,cs,b,bs,n) ->
            cooling (Seq.tail coolingSeq) (c,cs) (b,bs) n evalMax explorer objectiveF
        | Final(c,cs,b,bs,n) ->
            (n,b,bs)
        end

let optimize initial objective explorer evalMax coolingSeq =
    let _ = Random.self_init () in
    let cur = (initial, objective initial) in
    cooling coolingSeq cur cur 0 evalMax explorer objective

