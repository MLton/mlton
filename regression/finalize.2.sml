structure F = MLton.Finalizable

fun loop (n, f) =
   if n = 0
      then ()
   else
      let
         val f' = F.new n
         val _ = F.addFinalizer (f', fn _ =>
                                 F.withValue
                                 (f, fn n =>
                                  print (concat [Int.toString n, "\n"])))
      in
         loop (n - 1, f')
      end

val r = loop (10, F.new 13)

