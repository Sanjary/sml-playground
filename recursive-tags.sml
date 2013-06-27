val input = "Please, design a {function|program} that changes this {{beautiful|awesome} text|template} every time"
val simple = "Please, design a {function|program} that changes this {beautiful|awesome} text every time"

fun mkParser () =
    let
        val cvt  = SysWord.toInt o Posix.Process.pidToWord
        val seed = Random.rand (cvt (Posix.ProcEnv.getpid ()), cvt (Posix.ProcEnv.getppid ()))

        fun parse [] acc = acc
          | parse lst acc =
            let
                fun choose_random lst =
                    List.hd (List.drop (lst, Random.randRange (0, (List.length lst)-1) seed))
                fun parse_template lst =
                    let
                        fun parse_template' [] _ _ = raise Fail "Unmatched {"
                          | parse_template' (h::t) acc alt =
                            case h
                                of #"|" => parse_template' t "" (acc::alt)
                                |  #"}" => (acc::alt,t)
                                |  #"{" => let
                                             val (alt',t') = parse_template t
                                           in
                                             parse_template' t' (acc ^ choose_random alt') alt
                                           end
                                | _ => parse_template' t (acc ^ (Char.toString h)) alt
                    in
                        parse_template' lst "" []
                    end
            in
                case lst
                    of (#"\\")::(#"{")::t => parse t (acc ^ "\\{") (* support escaping *)
                    | #"{"::t => let
                                    val (alt,t') = parse_template t
                                  in
                                    parse t' (acc ^ choose_random alt)
                                  end
                    | h::t => parse t (acc ^ Char.toString h)
            end
    in
        fn s => parse (String.explode s) ""
    end
