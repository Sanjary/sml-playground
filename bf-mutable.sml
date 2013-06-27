datatype state = BF of {ptr : int ref,
                        cur : int ref,
                        mem : int array}

exception SyntaxError of string
exception RuntimeError of string

fun memval (BF{mem,ptr,...}) = Array.sub(mem, !ptr)

fun change_memory f (bf as BF{mem,ptr,...}) =
    Array.update(mem, !ptr, f(memval bf))

fun left (BF{ptr,cur,...}) =
    (ptr := !ptr - 1; cur := !cur + 1)

fun right (BF{ptr,cur,...}) =
    (ptr := !ptr + 1; cur := !cur + 1)

fun decr (bf as BF{cur,...}) =
    (cur := !cur + 1; change_memory (fn x => x-1) bf)

fun incr (bf as BF{cur,...}) =
    (cur := !cur + 1; change_memory (fn x => x+1) bf)

fun otpt (bf as BF{cur,...}) =
    (cur := !cur + 1; print (String.str (Char.chr (memval bf))))

fun inpt (bf as BF{cur,...}) =
    let
        fun getc () =
            case TextIO.inputLine TextIO.stdIn
                of NONE  => raise RuntimeError "Input read failed"
                | SOME s => case Char.fromString s
                                of NONE  => raise RuntimeError "Input string conversion failed"
                                | SOME c => Char.ord c
    in
        (cur := !cur + 1; change_memory (fn _ => getc()) bf)
    end

fun find_jump [] _ = raise RuntimeError "Corrupt jumps table"
  | find_jump ((j1,j2)::t) from =
        if from = j1
        then j2 (* jump forward *)
        else if from = j2
        then j1 (* jump backward *)
        else find_jump t from

fun loop_start (bf as BF{cur,...}) jmp =
    if memval bf = 0 then cur := find_jump jmp (!cur) else cur := !cur + 1

fun loop_stop (bf as BF{cur,...}) jmp =
    if memval bf > 0 then cur := find_jump jmp (!cur) else cur := !cur + 1

fun execute_one ch jmp bf =
    let
        fun run #"+" = incr bf
          | run #"-" = decr bf
          | run #"<" = left bf
          | run #">" = right bf
          | run #"." = otpt bf
          | run #"," = inpt bf
          | run #"[" = loop_start bf jmp
          | run #"]" = loop_stop bf jmp
          | run _    = raise SyntaxError "Unknown op"
    in
        (run ch; bf)
    end

fun execute_list lst jmp (bf as BF{cur,...}) =
    if !cur > List.length (lst) - 1
    then bf
    else execute_list lst jmp (execute_one (List.nth (lst, !cur)) jmp bf)

fun execute_string str =
    let
        fun precompute_loops [] _ = []
          | precompute_loops (h::t) pos =
            let
                fun find_closing_bracket [] _ _ = raise SyntaxError "Unpaired loop op"
                  | find_closing_bracket (#"["::t) stck pos = find_closing_bracket t (h::stck) pos+1
                  | find_closing_bracket (#"]"::t) stck pos = if null stck then pos else find_closing_bracket t (tl stck) pos+1
                  | find_closing_bracket (_::t) stck pos = find_closing_bracket t stck pos+1

                val next = pos+1
            in
                if h = #"["
                then (pos, (find_closing_bracket t [] next))::(precompute_loops t next)
                else precompute_loops t next
            end

        val bf = BF {cur = ref 0,
                     ptr = ref 0,
                     mem = Array.array(1000, 0)}
        and src = String.explode str
    in
        execute_list src (precompute_loops src 0) bf
    end
