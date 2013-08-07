(* http://habrahabr.ru/post/141228/ *)

(* non-greedy matching *)
fun segment_non_greedy d s =
    let
        fun find _ "" accum = SOME accum
          | find [] s accum = NONE
          | find (h::t) s accum =
                if String.isPrefix h s 
                then find t (String.extract (s, String.size h, NONE)) (accum @ [h])
                else find t s accum
    in
        find d s []
    end


