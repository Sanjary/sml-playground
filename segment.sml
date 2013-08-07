(* http://habrahabr.ru/post/141228/ *)

(* non-greedy matching *)
fun segment_non_greedy d s =
    let
        fun find _ "" accum = accum
          | find [] s accum = accum @ [s]
          | find (h::t) s accum =
                if String.isPrefix h s 
                then find t (String.extract (s, String.size h, NONE)) (accum @ [h])
                else find t s accum
    in
        find d s []
    end
