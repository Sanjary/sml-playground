fun eratosthenes n =
    let
        open BitArray

        val res = BitArray.array(n, true)
        and n' = Int.toLarge n

        fun crossout p =
            let
                val p' = Int.toLarge p
                fun crossout' idx =
                    if idx >= n'
                    then ()
                    else (BitArray.clrBit (res, (Int.fromLarge idx)); crossout' (idx+p'))
            in
                crossout' (p'*p');
                case List.filter (fn i => i > p) (BitArray.getBits res) (* implement find-first-set *)
                    of (h::_) => crossout h
                    | _ => ()
            end
    in
        (BitArray.clrBit (res, 0);
         BitArray.clrBit (res, 1);
         crossout 2;
         BitArray.getBits res)
    end
