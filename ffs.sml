(* ffs function for BitArray structure (comes with SMLofNJ) *)

(* find-first-set *)
fun ffs (BA{nbits=0,...}) = NONE
  | ffs (BA{bits,...}) =
    let
        (* count-trailing-zeroes *)
        fun ctz w =
            let
                (* population-count *)
                fun pop w =
                    let
                        fun pop' 0w0 cnt = cnt
                          | pop' w cnt = pop' (w & (w - 0w1)) (cnt+0w1)
                    in
                        Word8.toInt (pop' w 0w0)
                    end
            in
                pop ((w & (~ w)) - 0w1)
            end
    in
        case W8A.findi (fn (_,w) => w <> 0w0) bits
            of NONE => NONE
            | SOME (i,w) => SOME (i * Word8.wordSize + ctz w)
    end
