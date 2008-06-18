module Shuffle = struct

    type 'a t = {
        values: 'a array;
        mutable pos: int;
    }

    let create values = {
        values = Array.copy values;
        pos = 0;
    }

    let _shuffle sh =
        let len = Array.length sh.values in
        if sh.pos < len
        then
            begin
            let idx = Random.int (len - sh.pos) in
            let tmp = sh.values.(sh.pos + idx) in
            sh.values.(sh.pos + idx) <- sh.values.(sh.pos);
            sh.values.(sh.pos) <- tmp;
            sh.pos <- sh.pos + 1;
            sh
            end
        else
            failwith "index error"

    let rec get sh idx =
        if idx < sh.pos
        then
            sh.values.(idx)
        else
            get (_shuffle sh) idx

end (* module Shuffle *)


