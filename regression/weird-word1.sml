fun fib (w: Word5.word) : Word5.word =
   if w <= 0wx1
      then 0wx1
   else fib (w - 0wx1) + fib (w - 0wx2)

val s =
   case (fib 0wx5) of
      0wx0 => "0wx0"
    | 0wx1 => "0wx1"
    | 0wx2 => "0wx2"
    | 0wx3 => "0wx3"
    | 0wx4 => "0wx4"
    | 0wx5 => "0wx5"
    | 0wx6 => "0wx6"
    | 0wx7 => "0wx7"
    | 0wx8 => "0wx8"
    | 0wx9 => "0wx9"
    | 0wxA => "0wxA"
    | 0wxB => "0wxB"
    | 0wxC => "0wxC"
    | 0wxD => "0wxD"
    | 0wxE => "0wxE"
    | 0wxF => "0wxF"
    | _ => "zzz"

val _ = print (concat [s, "\n"])
