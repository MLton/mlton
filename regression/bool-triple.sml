val a = Array.array (100, (true, false, true))
val _ = Array.update (a, 0, (false, true, false))
val b =
   Array.sub (a, 0) = Array.sub (a, 1)
   andalso Array.sub (a, 2) = Array.sub (a, 3)
val _ = print (concat [Bool.toString b, "\n"])
