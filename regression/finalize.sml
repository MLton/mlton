structure F = MLton.Finalize

structure Weak = MLton.Weak

val n = 4
val rs = Array.tabulate (n, ref)
fun sub i = ! (Array.sub (rs, i))
val r = ref 13
fun clear i = Array.update (rs, i, r)
val () =
   Array.appi
   (fn (i, r) =>
    F.finalize (r, fn () =>
		print (concat [Int.toString i, " gone.\n"])))
   rs
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
