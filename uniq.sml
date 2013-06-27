(* O(N^2), order preserving *)
fun uniq lst =
    let
        fun uniq' [] accum = accum
          | uniq' (h::t) accum =
                if List.exists (fn x => h = x) t then uniq' t accum else uniq' t (accum @ [h])
    in
        uniq' lst []
    end

(* O(N), no order preserving *)
(* val uniq_int = uniq_faster (fn n => Word.fromInt n) (op =) *)
fun uniq_faster _ _ [] = []
  | uniq_faster h c lst =
        let
            exception Lookup
            open HashTable

            val ht = HashTable.mkTable (h, c) (List.length lst, Lookup)

            fun insert [] = ()
              | insert (h::t) = (HashTable.insert ht (h,1); insert t)
        in
            (insert lst; List.map (fn (k,_) => k) (HashTable.listItemsi ht))
        end

(* O(N), order preserving *)
(* val uniq_int_preserve = uniq_faster_preserve (fn n => Word.fromInt n) (op =) *)
fun uniq_faster_preserve _ _ [] = []
  | uniq_faster_preserve h c lst =
        let
            exception Lookup
            open HashTable

            val ht = HashTable.mkTable (h, c) (List.length lst, Lookup)

            fun insert [] accum = accum
              | insert (h::t) accum =
                    case HashTable.find ht h
                        of NONE  => (HashTable.insert ht (h,1); insert t (accum @ [h]))
                        | SOME _ => insert t accum
        in
            insert lst []
        end
