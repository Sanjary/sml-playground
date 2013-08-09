fun sliding_max n l =
    let
        fun sliding_window off = List.take (List.drop (l, off), n) handle Subscript => []
        val max = List.foldr (fn (crr,mx) => if crr > mx then crr else mx) (Option.valOf Int.minInt)
        fun loop start accum =
            case sliding_window start
                of [] => accum
                | win => loop (start+1) (accum @ [max win])
    in
        loop 0 []
    end
