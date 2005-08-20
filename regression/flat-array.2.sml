val n = 20

val a = Array.tabulate (n, fn i => (i, Array.array (1, 1)))

val (i, a') = Array.sub (a, 13)

val _ = Array.update (a', 0, i + Array.sub (a', 0))

val _ =
   print (concat [Int.toString (#1 (Array.sub (a, 12))), " ",
                  Int.toString (Array.sub (#2 (Array.sub (a, 13)), 0)), "\n"])
                  
