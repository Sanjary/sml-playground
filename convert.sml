fun convert base n =
    let
        val ord2str =  Char.toString o Char.chr o (fn n => n+55)
        fun digit n =
            if base < 37
            then
                if n < 10
                then Int.toString n
                else ord2str n
            else
                "(" ^ (Int.toString n) ^ ")"

        fun   convert' 0 true  = "0"
            | convert' 0 false = ""
            | convert' n _     = (convert' (n div base) false) ^ (digit (n mod base))
    in
        convert' n true
    end

val int2hex = convert 16
val int2oct = convert 8
val int2bin = convert 2
