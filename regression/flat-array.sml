val n = 20

val a = Array.tabulate (n, fn _ => (Array.array (1, 0),
                                    Array.array (1, 1)))

val (a1, a2) = Array.sub (a, 13)

val _ = Array.update (a1, 0, 2)
val _ = Array.update (a2, 0, 3)

val _ =
   print (concat [Int.toString (Array.sub (#1 (Array.sub (a, 12)), 0)), " ",
                  Int.toString (Array.sub (#2 (Array.sub (a, 13)), 0)), "\n"])
                  
