(* O(NM) where N - list size, M - window size *)
fun sliding_max n l =
    let
        fun sliding_window off = List.take (List.drop (l, off), n) handle Subscript => []
        val max = List.foldl (fn (crr,mx) => if crr > mx then crr else mx) (Option.valOf Int.minInt)
        fun loop start accum =
            case sliding_window start
                of [] => List.rev accum
                | win => loop (start+1) ((max win)::accum)
    in
        loop 0 []
    end


