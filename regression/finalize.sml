structure F = MLton.Finalizable

val n = 4
val fs = Array.tabulate (n, fn i =>
                         let
                            val f = F.new i
                            val _ = 
                               F.addFinalizer
                               (f, fn i =>
                                print (concat [Int.toString i, " gone.\n"]))
                         in
                            f
                         end)
fun sub i = F.withValue (Array.sub (fs, i), fn i => i)
val f = F.new 13
fun clear i = Array.update (fs, i, f)
val _ = clear 3
val _ = clear 2
val _ = MLton.GC.collect ()
fun pi x = print (concat [Int.toString x, "\n"])
val _ = pi (sub 0 + sub 1)
val _ = clear 1
val _ = MLton.GC.collect ()
val _ = pi (sub 0)
val _ = clear 0
val _ = MLton.GC.collect ()
