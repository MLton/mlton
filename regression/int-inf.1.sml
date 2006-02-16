val big: IntInf.int = 0x80000000

fun try (barg: IntInf.int): unit =
   let
      val bstr = IntInf.toString barg
      val _ = print (concat ["trying ", bstr, "\n"])
   in print (if ~ big <= barg
                then if barg < big
                        then "ok\n"
                     else "positive\n"
             else "negative\n")
   end

val _ = try 0
val _ = try 1
