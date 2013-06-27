fun anagrams s =
    let
        open HashTable HashString ListMergeSort

        exception Lookup

        val ht = HashTable.mkTable (HashString.hashString, (op =)) (101, Lookup)

        fun anagrams' [] = []
          | anagrams' (w::ws') =
            let
                val lc = String.implode o (List.map Char.toLower) o String.explode
                and norm = String.implode o (ListMergeSort.sort (op >)) o String.explode

                val l = lc w
                val k = norm l

                val found = (HashTable.lookup ht k) handle Lookup => []
            in
                if List.exists (fn x => x = l) found
                then anagrams' ws'
                else (HashTable.insert ht (k, l::found); anagrams' ws')
            end
    in
        anagrams' (String.tokens (not o Char.isAlpha) s);
        HashTable.filter (fn lst => List.length lst > 1) ht;
        HashTable.listItems ht
    end

fun input strm =
    let
        fun readline () =
            case TextIO.inputLine strm
                of NONE  => ""
                | SOME s => s

    in
        case TextIO.endOfStream strm
            of true => ""
            | false => (readline ()) ^ input strm
    end

val res = anagrams (input TextIO.stdIn)
