let (|>) left right =
    right left

module Array = struct
    include Array

    let rev a =
        let len = length a in
        let rget a n _ = a.(len - n - 1) in
        mapi (rget a) a

end

